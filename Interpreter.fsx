open System
open Checked

type ParameterMode =
    | PositionMode
    | ImmediateMode
    | RelativeMode

type Parameter = ParameterMode * int64

type Instruction =
    | Add of Parameter * Parameter * Parameter
    | Multiply of Parameter * Parameter * Parameter
    | Input of Parameter
    | Output of Parameter
    | JumpIfTrue of Parameter * Parameter
    | JumpIfFalse of Parameter * Parameter
    | LessThan of Parameter * Parameter * Parameter
    | Equals of Parameter * Parameter * Parameter
    | AdjustRelativeBase of Parameter
    | Halt

type ProgramState =
    | WaitingForInput of (int64 -> ProgramState)
    | OutputtingValue of int64 * (unit -> ProgramState)
    | Halted of int64 array

type Memory(initialData) =
    let mutable data = Array.copy initialData

    member __.Read(address : int64) =
        if int address >= data.Length then 0L else data.[int address]

    member __.Write(address : int64, value) =
        if int address >= data.Length then
            let newSize = max (int address + 1) (2 * data.Length)
            let newData = Array.zeroCreate<int64> newSize
            Array.blit data 0 newData 0 (data.Length)
            newData.[int address] <- value
            data <- newData
        else
            data.[int address] <- value

    member __.GetData() = data

    member __.CreateCopy() = Memory(data)

let getOpCode value = int value % 100

let getParameterMode (value : int64) (parameterPosition : int) =
    match (int value / pown 10 (parameterPosition + 1)) % 10 with
    | 0 -> PositionMode
    | 1 -> ImmediateMode
    | 2 -> RelativeMode
    | _ -> failwith "Invalid parameter mode"

let readInstruction (memory : Memory) ptr =
    let firstValue = memory.Read(ptr)
    let getParameter parameterPosition =
        let parameterMode = getParameterMode firstValue parameterPosition
        let parameterValue = memory.Read(ptr + int64 parameterPosition)
        (parameterMode, parameterValue)
    match getOpCode firstValue with
    | 1 -> Add (getParameter 1, getParameter 2, getParameter 3)
    | 2 -> Multiply (getParameter 1, getParameter 2, getParameter 3)
    | 3 -> Input (getParameter 1)
    | 4 -> Output (getParameter 1)
    | 5 -> JumpIfTrue (getParameter 1, getParameter 2)
    | 6 -> JumpIfFalse (getParameter 1, getParameter 2)
    | 7 -> LessThan (getParameter 1, getParameter 2, getParameter 3)
    | 8 -> Equals (getParameter 1, getParameter 2, getParameter 3)
    | 9 -> AdjustRelativeBase (getParameter 1)
    | 99 -> Halt
    | _ -> failwith "Invalid op code" 

let rec execute (memory : Memory) relativeBase ptr =
    let (|Resolved|) = function
        | PositionMode, position -> memory.Read(position)
        | ImmediateMode, value -> value
        | RelativeMode, relativePosition -> memory.Read(relativeBase + relativePosition)
    let (|OutputAddress|) = function
        | PositionMode, position -> position
        | ImmediateMode, _ -> failwith "Output position must not be in immidiate mode" 
        | RelativeMode, relativePosition -> relativeBase + relativePosition

    match readInstruction memory ptr with
    | Add (Resolved value1, Resolved value2, OutputAddress address) ->
        memory.Write(address, value1 + value2)
        execute memory relativeBase (ptr + 4L)

    | Multiply (Resolved value1, Resolved value2, OutputAddress address) ->
        memory.Write(address, value1 * value2)
        execute memory relativeBase (ptr + 4L)

    | Input (OutputAddress address) ->
        WaitingForInput (fun input ->
            let newMemory = memory.CreateCopy()
            newMemory.Write(address, input)
            execute newMemory relativeBase (ptr + 2L))

    | Output (Resolved value) ->     
        OutputtingValue (value, fun () ->
            execute (memory.CreateCopy()) relativeBase (ptr + 2L))

    | JumpIfTrue (Resolved value1, Resolved value2) ->
        execute memory relativeBase (if value1 <> 0L then value2 else ptr + 3L)

    | JumpIfFalse (Resolved value1, Resolved value2) ->
        execute memory relativeBase (if value1 = 0L then value2 else ptr + 3L)

    | LessThan (Resolved value1, Resolved value2, OutputAddress address) ->
        memory.Write(address, if value1 < value2 then 1L else 0L)
        execute memory relativeBase (ptr + 4L)

    | Equals (Resolved value1, Resolved value2, OutputAddress address) ->
        memory.Write(address, if value1 = value2 then 1L else 0L)
        execute memory relativeBase (ptr + 4L)

    | AdjustRelativeBase (Resolved value) ->
        execute memory (relativeBase + value) (ptr + 2L)

    | Halt ->
        Halted (memory.GetData())

let startProgram program =
    let memory = Memory(program)
    execute memory 0L 0L

let runProgram inputs program =
    let rec loop outputs remainingInputs programState =
        match programState, remainingInputs with
        | WaitingForInput _ , [] ->
            failwith "Run out of inputs"

        | WaitingForInput continuation, input :: tail ->
            loop outputs tail (continuation input)

        | OutputtingValue (value, continuation), _ ->
            loop (value :: outputs) remainingInputs (continuation ())

        | Halted memory, _ ->
            List.rev outputs, memory
    loop [] inputs (startProgram program)

let expectInput input = function
    | WaitingForInput continuation -> continuation input
    | OutputtingValue (value, _) -> failwithf "Expected input, but got output: %i" value
    | Halted _ -> failwith "Expected input, but program halted"
 
let expectOutput = function
    | WaitingForInput _ -> failwith "Expected output, but got input request"
    | OutputtingValue (output, continuation) -> (output, continuation ())
    | Halted _ -> failwith "Expected output, but program halted"
 
let expectHalt = function
    | WaitingForInput _ -> failwith "Expected halt, but got input request"
    | OutputtingValue (value, _) -> failwithf "Expected halt, but got output: %i" value
    | Halted data -> data

let expectManyInputs inputs programState =
    (programState, inputs)
    ||> List.fold (fun state input -> expectInput input state)

let runUntilInputOrHalt programState =
    let rec loop acc = function
        | OutputtingValue (output, continuation) ->
            loop (output :: acc) (continuation ())
        | nextProgramState ->
            (List.rev acc, nextProgramState)
    loop [] programState

let private withFormattedOutput (outputs : int64 list, programState) =
    let outputStrings = 
        outputs
        |> List.map (fun n -> if n > 127L then string n else char n |> string)
    (String.Join("", outputStrings), programState)

let startAsciiProgram program =
    runUntilInputOrHalt (startProgram program)
    |> withFormattedOutput

let continueAsciiProgram (command : string) programState =
    let inputs = 
        [ yield! command.ToCharArray(); yield '\n' ]
        |> List.map int64
    expectManyInputs inputs programState
    |> runUntilInputOrHalt
    |> withFormattedOutput

let runAsciiProgram program (commands : string list) =
    (startAsciiProgram program, commands)
    ||> List.scan (fun (_, programState) command ->
        continueAsciiProgram command programState)
    |> List.map fst
