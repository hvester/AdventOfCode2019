#load "Day1.fsx"
#load "Interpreter.fsx"

open System.IO
open Interpreter

let inputProgram =
    File.ReadAllText("data/input2.txt").Split([|','|])
    |> Array.map int

let result1 = runProgram inputProgram 12 2 |> Array.item 0 // 3716250

let verb, noun =
    seq {
        for input1 in 0 .. 99 do
            for input2 in 0 .. 99 do
                yield (input1, input2)
    }
    |> Seq.find (fun (input1, input2) ->
        (runProgram inputProgram input1 input2).[0] = 19690720)

let result2 = 100 * verb + noun // 6472