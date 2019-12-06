#load "Day1.fsx"
#load "Interpreter.fsx"

open System.IO
open Interpreter

let inputProgram =
    File.ReadAllText("data/input2.txt").Split([|','|])
    |> Array.map int

let io = Unchecked.defaultof<IO>

let result1 = runProgram inputProgram io (Some 12) (Some 2) |> Array.item 0 // 3716250

let verb, noun =
    seq {
        for input1 in 0 .. 99 do
            for input2 in 0 .. 99 do
                yield (input1, input2)
    }
    |> Seq.find (fun (input1, input2) ->
        (runProgram inputProgram io (Some input1) (Some input2)).[0] = 19690720)

let result2 = 100 * verb + noun // 6472