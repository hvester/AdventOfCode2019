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
    | 2L -> 'O'
    | _ -> failwith "Invalid output"

let executeMove direction programState =
    let cont1 = expectInput programState
    let command = commands.[direction]
    let output, cont2 = expectOutput (cont1 command)
    outputToChar output, cont2

let findOxygenSystem program =
    let mutable oxygenSystemPosition : (int * int) option = None
    let scene = Dictionary<int * int, char>()
    let startPosition = (0, 0)
    scene.[startPosition] <- '.'
    let rec loop steps states =
        let newStates =
            [ for position, programState in states do
                for direction in directions do
                    let targetPosition = move position direction
                    if not (scene.ContainsKey targetPosition) then
                        let c, cont2 = executeMove direction programState
                        scene.[targetPosition] <- c
                        if c = '.' then
                            yield (targetPosition, cont2())
                        elif c = 'O' then
                            oxygenSystemPosition <- Some targetPosition ]
        if oxygenSystemPosition.IsSome then
            steps + 1
        else
            loop (steps + 1) newStates
    loop 0 [ (startPosition, startProgram program) ]

let result1 = findOxygenSystem inputProgram
