let private getOperands (memory : int array) i =
    let outputPosition = memory.[i + 3]
    let operand1 = memory.[memory.[i + 1]]
    let operand2 = memory.[memory.[i + 2]]
    outputPosition, operand1, operand2

let runProgram program input1 input2 =
    let memory = Array.copy program
    memory.[1] <- input1
    memory.[2] <- input2
    let rec loop i =
        match memory.[i] with
        | 99 -> memory
        | 1 ->
            let outputPosition, operand1, operand2 = getOperands memory i
            memory.[outputPosition] <- operand1 + operand2
            loop (i + 4)
        | 2 ->
            let outputPosition, operand1, operand2 = getOperands memory i
            memory.[outputPosition] <- operand1 * operand2
            loop (i + 4)
        | _ ->
            failwith "Invalid op code"
    loop 0