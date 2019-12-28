#load "Interpreter.fsx"

open System.IO
open System.Collections.Generic
open Interpreter

let inputProgram = File.ReadAllText("data/input15.txt").Split([|','|]) |> Array.map int64

let directions, commands =
    let directionCommandPairs = [ ((0, 1), 1L); ((0, -1), 2L); ((-1, 0), 3L); ((1, 0), 4L) ]
    (List.map fst directionCommandPairs, dict directionCommandPairs)

let move (x, y) (dx, dy) = (x + dx, y + dy)

let outputToChar x =
    match x with
    | 0L -> '#'
    | 1L -> '.'
    | 2L -> 'S'
    | _ -> failwith "Invalid output"

let executeMove direction programState =
    let output, newProgramState = 
        programState
        |> expectInput commands.[direction]
        |> expectOutput
    (outputToChar output, newProgramState)

let exploreArea program =
    let area = Dictionary<int * int, char>()
    let distances = Dictionary<int * int, int>()
    let startPosition = (0, 0)
    area.[startPosition] <- '.'
    distances.[startPosition] <- 0
    let rec loop steps states =
        let newStates =
            [ for position, programState in states do
                for direction in directions do
                    let targetPosition = move position direction
                    if not (area.ContainsKey targetPosition) then
                        let c, newProgramState = executeMove direction programState
                        area.[targetPosition] <- c
                        if c = '.' || c = 'S' then
                            distances.[targetPosition] <- steps
                            yield (targetPosition, newProgramState) ]
        if List.isEmpty newStates then
            area, distances
        else
            loop (steps + 1) newStates
    loop 1 [ (startPosition, startProgram program) ]

let area, distances = exploreArea inputProgram

let oxygenSystemPosition =
    area
    |> Seq.pick (fun (KeyValue(position, c)) ->
        if c = 'S' then Some position else None)

let result1 = distances.[oxygenSystemPosition]

let renderArea position (area : Dictionary<int * int, char>) =
    let xs = area.Keys |> Seq.map fst
    let ys = area.Keys |> Seq.map snd
    for y in Seq.max ys .. -1 .. Seq.min ys do
        for x in Seq.min xs .. Seq.max xs do
            let c =
                if (x, y) = position then
                    'D'
                else 
                    match area.TryGetValue <| (x, y) with
                    | false, _ -> ' '
                    | true, c -> c
            printf "%c" c
        printfn ""

// renderArea (0, 0) area

let fillArea startPosition (area : Dictionary<int * int, char>) =
    let hasOxygen = HashSet<int * int>()
    hasOxygen.Add(startPosition) |> ignore
    let rec loop steps positions =
        let newPositions =
            [ for position in positions do
                for direction in directions do
                    let targetPosition = move position direction
                    if not (hasOxygen.Contains(targetPosition)) && area.[targetPosition] = '.' then
                        hasOxygen.Add(targetPosition) |> ignore
                        yield targetPosition ]
        if List.isEmpty newPositions then
            steps
        else
            loop (steps + 1) newPositions
    loop 0 [ startPosition ]

let result2 = fillArea oxygenSystemPosition area
