#load "Interpreter.fsx"

open System
open System.IO
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

let startPosition =
    positions
    |> Seq.pick (fun (KeyValue(position, c)) ->
        if c = '^' then Some position else None)

let left (dx, dy) = (dy, -dx)
let right (dx, dy) = (-dy, dx)
let move (x, y) (dx, dy) = (x + dx, y + dy)

type Move =
    | Left
    | Right
    | Forward of int

let route =
    let rec loop position direction steps =
        seq {
            if charAt (move position direction) = Some '#' then
                yield! loop (move position direction) direction (steps + 1)
            elif charAt (move position (left direction)) = Some '#' then
                if steps > 0 then yield Forward steps
                yield Left
                yield! loop (move position (left direction)) (left direction) 1
            elif charAt (move position (right direction)) = Some '#' then
                if steps > 0 then yield Forward steps
                yield Right
                yield! loop (move position (right direction)) (right direction) 1
            else
                if steps > 0 then yield Forward steps 
        }
    loop startPosition (0, -1) 0
    |> Seq.toList

let tryFunction func xs =
    let part, rest = List.splitAt (List.length func) xs
    if part = func then Some rest else None

let tryOrDefineFunction funcLength funcOpt xs =
    if List.length xs < funcLength then
        None
    else
        match funcOpt with
        | Some func ->
            tryFunction func xs
            |> Option.map (fun rest -> (func, rest) )
        | None ->
            let func, rest =
                List.splitAt funcLength xs
            Some (func, rest)

let tryfunctionLengths a b c xs =
    let rec loop acc aFuncOpt bFuncOpt cFuncOpt remaining =
        if List.isEmpty remaining then
            Some (List.rev acc, aFuncOpt, bFuncOpt, cFuncOpt)
        else
            match tryOrDefineFunction a aFuncOpt remaining with
            | Some (func, rest) ->
                loop ("A" :: acc) (Some func) bFuncOpt cFuncOpt rest
            | None ->
                match tryOrDefineFunction b bFuncOpt remaining with
                | Some (func, rest) ->
                    loop ("B" :: acc) aFuncOpt (Some func) cFuncOpt rest
                | None ->
                    match tryOrDefineFunction c cFuncOpt remaining with
                    | Some (func, rest) ->
                        loop ("C" :: acc) aFuncOpt bFuncOpt (Some func) rest
                    | None ->
                        None
    loop [] None None None xs

let mainRoutine, aFuncOpt, bFuncOpt, cFuncOpt =
    seq {
        for a in 1 .. 10 do
            for b in 1 .. 10 do
                for c in 1 .. 10 do
                    yield tryfunctionLengths a b c route
    }
    |> Seq.pick id

let mainRoutineInputs = String.Join(",", mainRoutine)

let funcToInputs func =
    let strings =
        func
        |> List.map (function
            | Left -> "L"
            | Right -> "R"
            | Forward x -> string x)
    String.Join(",", strings)

let aFuncInputs = funcToInputs aFuncOpt.Value
let bFuncInputs = funcToInputs bFuncOpt.Value
let cFuncInputs = funcToInputs cFuncOpt.Value

let fixedProgram =
    let program = Array.copy inputProgram
    program.[0] <- 2L
    program

let commands =
    [ mainRoutineInputs
      aFuncInputs
      bFuncInputs
      cFuncInputs
      "n" ]

let result2 =
    runAsciiProgram fixedProgram commands
    |> List.last
    |> fun str -> str.Split('\n') |> Array.last |> int64
