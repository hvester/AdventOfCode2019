#load "Interpreter.fsx"

open System.IO
open Interpreter

let inputProgram =
    File.ReadAllText("data/input2.txt").Split([|','|])
    |> Array.map int

let io = Unchecked.defaultof<IO>

let fixProgram input1 input2 program =
    let programCopy = Array.copy program
    programCopy.[1] <- input1
    programCopy.[2] <- input2
    programCopy

let result1 =
    inputProgram
    |> fixProgram 12 2
    |> runProgram io
    |> Array.item 0 // 3716250

let verb, noun =
    seq {
        for input1 in 0 .. 99 do
            for input2 in 0 .. 99 do
                yield (input1, input2)
    }
    |> Seq.find (fun (input1, input2) ->
        inputProgram
        |> fixProgram input1 input2
        |> runProgram io
        |> Array.item 0
        |> ((=) 19690720))

let result2 = 100 * verb + noun // 6472