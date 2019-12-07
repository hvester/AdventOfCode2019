#load "Interpreter.fsx"

open System.IO
open Interpreter

let inputProgram =
    File.ReadAllText("data/input7.txt").Split([|','|])
    |> Array.map int

let runAmplifiers program settings =
    (0, settings)
    ||> List.fold (fun input setting ->
        runProgram (inputOfSeq [setting; input]) program
        |> fst
        |> List.head)

let rec getAllPermutations xs =
    [|
        if Array.isEmpty xs then
            yield [||]
        else
            for i in 0 .. xs.Length - 1 do
                let x = xs.[i]
                let rest =
                    Array.init (xs.Length - 1) (fun j ->
                        if j < i then xs.[j] else xs.[j+1])
                for permutation in getAllPermutations rest do
                    yield Array.append [| x |] permutation
    |]

let result1 =
    getAllPermutations [| 0 .. 4 |]
    |> Array.map (Array.toList >> runAmplifiers inputProgram)
    |> Array.max
