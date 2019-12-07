#load "Interpreter.fsx"

open System.IO
open Interpreter

let program = File.ReadAllText("data/input5.txt").Split([|','|]) |> Array.map int

let result1 =
    runProgram [ 1 ] program
    |> fst
    |> List.skipWhile ((=) 0)
    |> function
        | [ diagnosticCode ] -> diagnosticCode
        | _ -> failwith "Something went wrong"

let result2 =
    runProgram [ 5 ] program
    |> function
        | [ diagnosticCode ], _ -> diagnosticCode
        | _ -> failwith "Something went wrong"
