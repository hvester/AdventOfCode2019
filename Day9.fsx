#load "Interpreter.fsx"

open System.IO
open Interpreter

let program = File.ReadAllText("data/input9.txt").Split([|','|]) |> Array.map int64

let result1 = runProgram [ 1L ] program |> fst |> List.exactlyOne

let result2 = runProgram [ 2L ] program |> fst |> List.exactlyOne
