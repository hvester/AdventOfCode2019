#load "Interpreter.fsx"

open System.IO
open Interpreter

let program = File.ReadAllText("data/input5.txt").Split([|','|]) |> Array.map int64

let result1 =
    runProgram [ 1L ] program
    |> fst
    |> List.skipWhile ((=) 0L)
    |> function
        | [ diagnosticCode ] -> diagnosticCode
        | _ -> failwith "Something went wrong"

let result2 =
    runProgram [ 5L ] program
    |> function
        | [ diagnosticCode ], _ -> diagnosticCode
        | _ -> failwith "Something went wrong"
