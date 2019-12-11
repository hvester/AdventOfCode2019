#load "Interpreter.fsx"
#load "Debugger.fsx"

open System
open System.IO
open Interpreter
open Debugger

let inputProgram = File.ReadAllText("data/input11.txt").Split([|','|]) |> Array.map int64

let getNextPositionAndDirection (x, y) (dx, dy) turn =
    let t = 2 * (int turn) - 1
    let (newDx, newDy) as newDirection = (t * dy, -t * dx)
    let newPosition = (x + newDx, y + newDy)
    (newPosition, newDirection)

let runPaintingRobot initialPanels program =
    let rec loop panels position direction = function
        | WaitingForInput continuation1 ->
            let color =
                match Map.tryFind position panels with
                | Some c -> c
                | None -> 0
            match continuation1 (int64 color) with
            | OutputtingValue (newColor, continuation2) ->
                let newPanels = Map.add position (int newColor) panels
                match continuation2 () with
                | OutputtingValue (turn, continuation3) ->
                    let newPosition, newDirection =
                        getNextPositionAndDirection position direction turn
                    loop newPanels newPosition newDirection (continuation3 ())
                | _ ->
                    failwith "Unexpected program state"
            | _ ->
                failwith "Unexpected program state"

        | Halted _ ->
            panels
        | _ ->
            failwith "Unexpected program state"
    loop initialPanels (0, 0) (0, 1) (startProgram None program)

let result1 = (runPaintingRobot Map.empty inputProgram).Count // 2720


let renderPanels panels =
    let xs, ys = Map.toList panels |> List.map fst |> List.unzip
    let minx, maxx = List.min xs, List.max xs
    let miny, maxy = List.min ys, List.max ys
    let lines =
        [ for y in maxy .. -1 .. miny do
            let chars =
                [ for x in minx .. maxx do
                    match Map.tryFind (x, y) panels with
                    | Some 1 -> yield "#"
                    | _ -> yield "." ]
            yield String.Join("", chars) ]
    String.Join("\n", lines)

let result2 =
    runPaintingRobot (Map.ofList [ ((0, 0), 1) ]) inputProgram
    |> renderPanels
    
