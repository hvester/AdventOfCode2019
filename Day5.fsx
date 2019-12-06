#load "Interpreter.fsx"

open System.IO
open Interpreter

let program = File.ReadAllText("data/input5.txt").Split([|','|]) |> Array.map int

let runWithInput input program =
    let output = ResizeArray<_>()
    let io =
        { new IO with
            member __.Input() = input
            member __.Output(v) = output.Add(v) }
    runProgram io program |> ignore
    Seq.toList output

let result1 =
    runWithInput 1 program
    |> List.skipWhile ((=) 0)
    |> function
        | [ diagnosticCode ] -> diagnosticCode
        | _ -> failwith "Something went wrong"

let result2 =
    runWithInput 5 program
    |> function
        | [ diagnosticCode ] -> diagnosticCode
        | _ -> failwith "Something went wrong"
