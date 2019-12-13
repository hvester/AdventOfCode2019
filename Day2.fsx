#load "Interpreter.fsx"

open System.IO
open Interpreter

let inputProgram =
    File.ReadAllText("data/input2.txt").Split([|','|])
    |> Array.map int64

let fixProgram input1 input2 program =
    let programCopy = Array.copy program
    programCopy.[1] <- input1
    programCopy.[2] <- input2
    programCopy

let result1 =
    inputProgram
    |> fixProgram 12L 2L
    |> runProgram []
    |> snd
    |> Array.item 0 // 3716250

let verb, noun =
    seq {
        for input1 in 0L .. 99L do
            for input2 in 0L .. 99L do
                yield (input1, input2)
    }
    |> Seq.find (fun (input1, input2) ->
        inputProgram
        |> fixProgram input1 input2
        |> runProgram []
        |> snd
        |> Array.item 0
        |> ((=) 19690720L))

let result2 = 100L * verb + noun // 6472