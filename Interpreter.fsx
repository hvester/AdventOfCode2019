open System.Collections.Generic

type ParameterMode =
    | PositionMode
    | ImmediateMode

type Parameter = ParameterMode * int

type Instruction =
    | Add of Parameter * Parameter * int 
    | Multiply of Parameter * Parameter * int
    | Input of int
    | Output of Parameter
    | JumpIfTrue of Parameter * Parameter
    | JumpIfFalse of Parameter * Parameter
    | LessThan of Parameter * Parameter * int
    | Equals of Parameter * Parameter * int
    | Halt

type ProgramState =
    | WaitingForInput of (int -> ProgramState)
    | OutputtingValue of int * (unit -> ProgramState)
    | Halted of int array

let getOpCode value = value % 100

let getParameterMode value parameterPosition =
    match (value / pown 10 (parameterPosition + 1)) % 10 with
    | 0 -> PositionMode
    | 1 -> ImmediateMode
    | _ -> failwith "Invalid parameter mode"

let readInstruction (memory : int array) ptr =
    let firstValue = memory.[ptr]
    let getParameterValue parameterPosition =
        memory.[ptr + parameterPosition]
    let getParameter parameterPosition =
        let parameterMode = getParameterMode firstValue parameterPosition
        let parameterValue = getParameterValue parameterPosition
        (parameterMode, parameterValue)
    match getOpCode firstValue with
    | 1 -> Add (getParameter 1, getParameter 2, getParameterValue 3)
    | 2 -> Multiply (getParameter 1, getParameter 2, getParameterValue 3)
    | 3 -> Input (getParameterValue 1)
    | 4 -> Output (getParameter 1)
    | 5 -> JumpIfTrue (getParameter 1, getParameter 2)
    | 6 -> JumpIfFalse (getParameter 1, getParameter 2)
    | 7 -> LessThan (getParameter 1, getParameter 2, getParameterValue 3)
    | 8 -> Equals (getParameter 1, getParameter 2, getParameterValue 3)
    | 99 -> Halt
    | _ -> failwith "Invalid op code" 

let rec execute (memory : int array) ptr =
    let (|Resolved|) = function
        | PositionMode, position -> memory.[position]
        | ImmediateMode, value -> value

    match readInstruction memory ptr with
    | Add (Resolved value1, Resolved value2, outputPosition) ->
        memory.[outputPosition] <- value1 + value2
        execute memory (ptr + 4)

    | Multiply (Resolved value1, Resolved value2, outputPosition) ->
        memory.[outputPosition] <- value1 * value2
        execute memory (ptr + 4)

    | Input position ->
        WaitingForInput (fun input ->
            memory.[position] <- input
            execute memory (ptr + 2))

    | Output (Resolved value) ->     
        OutputtingValue (value, fun () -> execute memory (ptr + 2))

    | JumpIfTrue (Resolved value1, Resolved value2) ->
        execute memory (if value1 <> 0 then value2 else ptr + 3)

    | JumpIfFalse (Resolved value1, Resolved value2) ->
        execute memory (if value1 = 0 then value2 else ptr + 3)

    | LessThan (Resolved value1, Resolved value2, outputPosition) ->
        let outputValue = if value1 < value2 then 1 else 0
        memory.[outputPosition] <- outputValue
        execute memory (ptr + 4)

    | Equals (Resolved value1, Resolved value2, outputPosition) ->
        let outputValue = if value1 = value2 then 1 else 0
        memory.[outputPosition] <- outputValue
        execute memory (ptr + 4)

    | Halt ->
        Halted memory

let startProgram program =
    let memory = Array.copy program
    execute memory 0

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

