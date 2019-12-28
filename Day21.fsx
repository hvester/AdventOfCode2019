#load "Interpreter.fsx"

open System.IO
open Interpreter

let inputProgram = File.ReadAllText("data/input21.txt").Split([|','|]) |> Array.map int64

let getResult (output : string list) =
    (List.last output).Split('\n') |> Array.last |> int

let result1 =
    [ // (A) Jump if three forward is hole but four forward is not hole
      // Result -> J 
      "NOT C J"
      "AND D J"
      // (B) Jump if two forward is hole but four forward is not hole 
      // Result -> T
      "NOT B T"
      "AND D T"
      // Combine A and B
      // Result -> T
      "OR J T"
      // Jump if next is hole
      "NOT A J"
      "OR T J"
      "WALK" ]
    |> runAsciiProgram inputProgram
    |> getResult

let result2 =
    [ // (A) Jump if three forward is hole but four forward is not hole
      // Result -> J 
      "NOT C J"
      "AND D J"
      // (B) Jump if two forward is hole but four forward is not hole 
      // Result -> T
      "NOT B T"
      "AND D T"
      // Combine A and B
      // Result -> T
      "OR J T"
      // Jump if next is hole
      // Result -> J
      "NOT A J"
      "OR T J"
      // Except if fifth and eighth are holes
      "NOT E T" // If 5th is hole T is true
      "NOT T T" // If 5th is hole T is false
      "OR H T"  // If 8th is hole (false) and 5th is hole T false
      "AND T J"
      "RUN" ]
    |> runAsciiProgram inputProgram
    |> getResult
