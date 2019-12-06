type ParameterMode =
    | PositionMode
    | ImmediateMode

type Parameter = ParameterMode * int

type Instruction =
    | Add of Parameter * Parameter * int 
    | Multiply of Parameter * Parameter * int
    | Input of int
    | Output of Parameter
    | Halt

type IO =
    abstract Input : unit -> int
    abstract Output : int -> unit

let getOpCode value = value % 100

let getParameterMode value parameterPosition =
    match (value / pown 10 (parameterPosition + 1)) % 10 with
    | 0 -> PositionMode
    | 1 -> ImmediateMode
    | _ -> failwith "Invalid parameter mode"

let readInstruction (memory : int array) instructionPointer =
    let firstValue = memory.[instructionPointer]
    let getParameterValue parameterPosition =
        memory.[instructionPointer + parameterPosition]
    let getParameter parameterPosition =
        let parameterMode = getParameterMode firstValue parameterPosition
        let parameterValue = getParameterValue parameterPosition
        (parameterMode, parameterValue)
    match getOpCode firstValue with
    | 1 -> Add (getParameter 1, getParameter 2, getParameterValue 3)
    | 2 -> Multiply (getParameter 1, getParameter 2, getParameterValue 3)
    | 3 -> Input (getParameterValue 1)
    | 4 -> Output (getParameter 1)
    | 99 -> Halt
    | _ -> failwith "Invalid op code" 

let getNewInstructionPointer instruction instructionPointer =
    let shift =
        match instruction with
        | Add _ | Multiply _ -> 4
        | Input _ | Output _ -> 2
        | Halt _ -> 1
    instructionPointer + shift

let resolveParameter (memory : int array) parameter =
    match parameter with
    | PositionMode, position -> memory.[position]
    | ImmediateMode, value -> value

let executeInstruction memory (io : IO) instruction =
    match instruction with
    | Add (parameter1, parameter2, outputPosition) ->
        let value1 = resolveParameter memory parameter1
        let value2 = resolveParameter memory parameter2
        memory.[outputPosition] <- value1 + value2
    | Multiply (parameter1, parameter2, outputPosition) ->
        let value1 = resolveParameter memory parameter1
        let value2 = resolveParameter memory parameter2
        memory.[outputPosition] <- value1 * value2
    | Input position ->
        memory.[position] <- io.Input ()
    | Output parameter ->
        let value = resolveParameter memory parameter
        io.Output value
    | Halt ->
        ()
    match instruction with | Halt -> true | _ -> false

let runProgram io program =
    let memory = Array.copy program
    let rec loop instructionPointer =
        let instruction = readInstruction memory instructionPointer
        match executeInstruction memory io instruction with
        | true ->
            memory
        | false -> 
            loop (getNewInstructionPointer instruction instructionPointer)
    loop 0
