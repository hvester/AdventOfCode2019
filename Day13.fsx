#load "Interpreter.fsx"

open System.IO
open Interpreter

let inputProgram = File.ReadAllText("data/input13.txt").Split([|','|]) |> Array.map int64

let result1 =
    runProgram [] inputProgram
    |> fst
    |> List.chunkBySize 3
    |> List.filter (fun xs -> xs.[2] = 2L)
    |> List.length
