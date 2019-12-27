#load "Interpreter.fsx"

open System.IO
open Interpreter

let inputProgram = File.ReadAllText("data/input21.txt").Split([|','|]) |> Array.map int64

let commands =
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

let result1 =
    let lastOutput =
        runAsciiProgram commands inputProgram
        |> List.last
    lastOutput.Split('\n')
    |> Array.last
    |> int
