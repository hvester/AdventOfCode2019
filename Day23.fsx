#load "Interpreter.fsx"

open System.IO
open System.Collections.Generic
open Interpreter

let nicSoftware = File.ReadAllText("data/input23.txt").Split([|','|]) |> Array.map int64

let runNetwork () =
    let packetQueues = Array.init 50 (fun _ -> Queue<int64 * int64>())
    let programStates =
        Array.init 50 (fun i ->
            startProgram nicSoftware
            |> expectInput (int64 i))
    let rec loop i =
        match programStates.[i] with
        | WaitingForInput _ ->
            let newProgramState = 
                if packetQueues.[i].Count = 0 then
                    expectInput -1L programStates.[i]
                else
                    let x, y = packetQueues.[i].Dequeue()
                    programStates.[i]
                    |> expectInput x
                    |> expectInput y
            programStates.[i] <- newProgramState
            loop ((i + 1) % 50)
        | OutputtingValue _ ->
            let address, state1 = expectOutput programStates.[i]
            let x, state2 = expectOutput state1
            let y, newProgramState = expectOutput state2
            if address = 255L then
                y
            else
                packetQueues.[int address].Enqueue(x, y)
                programStates.[i] <- newProgramState
                loop ((i + 1) % 50)
        | Halted _ ->
            failwithf "%i halted" i
    loop 0

let result1 = runNetwork ()