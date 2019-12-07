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

type IO =
    abstract Input : unit -> int
    abstract Output : int -> unit

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

let resolveParameter (memory : int array) parameter =
    match parameter with
    | PositionMode, position -> memory.[position]
    | ImmediateMode, value -> value

let executeInstruction memory input output ptr instruction =
    match instruction with
    | Add (parameter1, parameter2, outputPosition) ->
        let value1 = resolveParameter memory parameter1
        let value2 = resolveParameter memory parameter2
        memory.[outputPosition] <- value1 + value2
        false, ptr + 4

    | Multiply (parameter1, parameter2, outputPosition) ->
        let value1 = resolveParameter memory parameter1
        let value2 = resolveParameter memory parameter2
        memory.[outputPosition] <- value1 * value2
        false, ptr + 4

    | Input position ->
        memory.[position] <- input ()
        false, ptr + 2

    | Output parameter ->
        let value = resolveParameter memory parameter
        output value
        false, ptr + 2

    | JumpIfTrue (parameter1, parameter2) ->
        let value1 = resolveParameter memory parameter1
        let value2 = resolveParameter memory parameter2
        if value1 <> 0 then
            false, value2
        else
            false, ptr + 3

    | JumpIfFalse (parameter1, parameter2) ->
        let value1 = resolveParameter memory parameter1
        let value2 = resolveParameter memory parameter2
        if value1 = 0 then
            false, value2
        else
            false, ptr + 3

    | LessThan (parameter1, parameter2, outputPosition) ->
        let value1 = resolveParameter memory parameter1
        let value2 = resolveParameter memory parameter2
        let outputValue = if value1 < value2 then 1 else 0
        memory.[outputPosition] <- outputValue
        false, ptr + 4

    | Equals (parameter1, parameter2, outputPosition) ->
        let value1 = resolveParameter memory parameter1
        let value2 = resolveParameter memory parameter2
        let outputValue = if value1 = value2 then 1 else 0
        memory.[outputPosition] <- outputValue
        false, ptr + 4

    | Halt ->
        true, 0

let runProgram input program =
    let memory = Array.copy program
    let outputValues = ResizeArray<int>()
    let output = outputValues.Add
    let rec loop ptr =
        let instruction = readInstruction memory ptr
        match executeInstruction memory input output ptr instruction with
        | true, _ ->
            ()
        | false, newPtr -> 
            loop newPtr
    loop 0
    Seq.toList outputValues, memory

let inputOfSeq (inputs : int seq) =
    let enumerator = inputs.GetEnumerator()
    fun () ->
        enumerator.MoveNext() |> ignore
        enumerator.Current
