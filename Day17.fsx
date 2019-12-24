#load "Interpreter.fsx"

open System.IO
open System.Collections.Generic
open Interpreter

let inputProgram = File.ReadAllText("data/input17.txt").Split([|','|]) |> Array.map int64

let scene = runProgram [] inputProgram |> fst |> List.map char

let positions =
    ((0, 0), scene)
    ||> List.mapFold (fun (x, y) c ->
        let nextPosition = if c = '\n' then (0, y + 1) else (x + 1, y)
        (((x, y), c), nextPosition) )
    |> fst
    |> dict

let charAt (x, y) =
    match positions.TryGetValue <| (x, y) with
    | false, _ -> None
    | true, c -> Some c

let result1 =
    positions
    |> Seq.sumBy (fun (KeyValue((x, y), c)) ->
        match charAt (x, y), charAt (x - 1, y), charAt (x + 1, y), charAt (x, y + 1), charAt (x, y - 1) with
        | Some '#', Some '#', Some '#', Some '#', Some '#' -> x * y
        | _ -> 0)
