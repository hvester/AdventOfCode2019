#load "Interpreter.fsx"

open System.IO
open Interpreter

let program = File.ReadAllText("data/input5.txt").Split([|','|]) |> Array.map int

let result1 =
    let output = ResizeArray<_>()
    let io =
        { new IO with
            member __.Input() = 1
            member __.Output(v) = output.Add(v) }
    runProgram program io None None |> ignore
    match output |> Seq.skipWhile ((=) 0) |> Seq.toList with
    | [ diagnosticCode ] -> diagnosticCode
    | _ -> failwith "Something went wrong"
// 5577461