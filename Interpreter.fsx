type ParameterMode =
    | PositionMode
    | ImmediateMode

type Parameter = ParameterMode * int

type Instruction =
    | Add of Parameter * Parameter * int 
    | Multiply of Parameter * Parameter * int
    | Halt

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
    | 99 -> Halt
    | _ -> failwith "Invalid op code" 

let getNewInstructionPointer instruction instructionPointer =
    let shift =
        match instruction with
        | Add _ | Multiply _ -> 4
        | Halt _ -> 1
    instructionPointer + shift

let resolveParameter (memory : int array) parameter =
    match parameter with
    | PositionMode, position -> memory.[position]
    | ImmediateMode, value -> value

let executeInstruction memory instruction =
    match instruction with
    | Add (parameter1, parameter2, outputPosition) ->
        let value1 = resolveParameter memory parameter1
        let value2 = resolveParameter memory parameter2
        memory.[outputPosition] <- value1 + value2
    | Multiply (parameter1, parameter2, outputPosition) ->
        let value1 = resolveParameter memory parameter1
        let value2 = resolveParameter memory parameter2
        memory.[outputPosition] <- value1 * value2
    | Halt ->
        ()
    match instruction with | Halt -> true | _ -> false

let runProgram program input1 input2 =
    let memory = Array.copy program
    memory.[1] <- input1
    memory.[2] <- input2
    let rec loop instructionPointer =
        let instruction = readInstruction memory instructionPointer
        match executeInstruction memory instruction with
        | true ->
            memory
        | false -> 
            loop (getNewInstructionPointer instruction instructionPointer)
    loop 0