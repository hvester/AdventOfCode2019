#load "Interpreter.fsx"

open System
open System.IO
open Interpreter

let inputProgram = File.ReadAllText("data/input13.txt").Split([|','|]) |> Array.map int64

let result1 =
    runProgram [] inputProgram
    |> fst
    |> List.chunkBySize 3
    |> List.filter (fun xs -> xs.[2] = 2L)
    |> List.length

let hackedProgram =
    let program = Array.copy inputProgram
    program.[0] <- 2L
    program

let renderScene gameState =
    let coordinates =
        Map.toSeq gameState
        |> Seq.map fst
        |> Seq.filter (fun v -> v <> (-1, 0))
    let maxx = coordinates |> Seq.map fst |> Seq.max
    let maxy = coordinates |> Seq.map snd |> Seq.max
    let score =
        match Map.tryFind (-1, 0) gameState with
        | None -> ""
        | Some s -> s
    printfn "Score: %s" score
    for y in 0 .. maxy do
        for x in 0 .. maxx do
            match Map.tryFind (x, y) gameState with
            | None -> printf " "
            | Some s -> printf "%s" s 
        printfn ""

let tileOfValue = function
    | 0 -> " "
    | 1 -> "*"
    | 2 -> "#"
    | 3 -> "T"
    | 4 -> "o"
    | x -> string x

let updateGameState gameState newData =
    (gameState, List.chunkBySize 3 newData)
    ||> List.fold (fun state dataChunk ->
        match dataChunk with
        | [ x; y; value ] ->
            Map.add (x, y) (tileOfValue value) state
        | _ ->
            failwith "Invalid data")

let runGame input output program =
    let processOutputs gameState outputStack =
        let outputs = outputStack |> List.rev |> List.map int
        let newGameState = updateGameState gameState outputs
        output newGameState
        newGameState
    let rec loop acc gameState = function
        | OutputtingValue (value, cont) ->
            loop (value :: acc) gameState (cont ())
        | WaitingForInput cont ->
            let newGameState = processOutputs gameState acc
            let joystickState = input newGameState
            loop [] newGameState (cont joystickState)
        | Halted _ ->
            processOutputs gameState acc
            |> Map.find (-1, 0)
            |> int
    loop [] Map.empty (startProgram program)

let playGameInteractively program =
    let input _ =
        let cki = Console.ReadKey()
        match cki.Key with
        | ConsoleKey.LeftArrow -> -1L
        | ConsoleKey.UpArrow ->    0L
        | ConsoleKey.RightArrow -> 1L
        | _ -> failwith "Quitting game"
    let score = runGame input renderScene program
    printfn "Your score: %i" score

// playGameInteractively hackedProgram |> ignore


let playGame program =
    let input gameState =
        let findSymbolX symbol =
            Map.toSeq gameState
            |> Seq.pick (fun ((x, _), v) ->
                if v = symbol then Some x else None)
        let ball = findSymbolX "o"
        let paddle = findSymbolX "T"
        if ball > paddle then 1L elif ball < paddle then -1L else 0L
    runGame input ignore program

let result2 = playGame hackedProgram
