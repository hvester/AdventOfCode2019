#load "Interpreter.fsx"

open System.IO
open Interpreter

let inputProgram =
    File.ReadAllText("data/input7.txt").Split([|','|])
    |> Array.map int

let runAmplifiers program settings =
    (0, settings)
    ||> List.fold (fun input setting ->
        runProgram [setting; input] program
        |> fst
        |> List.head)

let rec getAllPermutations xs =
    [|
        if Array.isEmpty xs then
            yield [||]
        else
            for i in 0 .. xs.Length - 1 do
                let x = xs.[i]
                let rest =
                    Array.init (xs.Length - 1) (fun j ->
                        if j < i then xs.[j] else xs.[j+1])
                for permutation in getAllPermutations rest do
                    yield Array.append [| x |] permutation
    |]

let result1 =
    getAllPermutations [| 0 .. 4 |]
    |> Array.map (Array.toList >> runAmplifiers inputProgram)
    |> Array.max

let runAmplifiersWithFeedbackLoop program settings =
    let initializedPrograms =
        settings
        |> List.map (fun setting ->
            match startProgram program with
            | WaitingForInput continuation ->
                continuation setting
            | _ -> failwith "Invalid program")
    let runFeedBackLoop programStates loopInput =
        (Some loopInput, programStates)
        ||> List.mapFold (fun inputOpt programState ->
            match programState, inputOpt with
            | WaitingForInput continuation, Some input ->
                match continuation input with
                | OutputtingValue (outputValue, continuation) ->
                    continuation (), Some outputValue
                | ps ->
                    failwithf "1. Invalid program: %A" ps
            | Halted memory, _ ->
                Halted memory, None
            | ps ->
                failwithf "2. Invalid program: %A" ps)
    let rec loop programStates loopInput =
        match runFeedBackLoop programStates loopInput with
        | nextProgramStates, Some loopOutput ->
            loop nextProgramStates loopOutput
        | _, None ->
            loopInput
    loop initializedPrograms 0

let result2 =
    getAllPermutations [| 5 .. 9 |]
    |> Array.map (Array.toList >> runAmplifiersWithFeedbackLoop inputProgram)
    |> Array.max

