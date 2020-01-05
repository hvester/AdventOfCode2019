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
    let rec loop i natState nIdleComputers =
        seq {
            if nIdleComputers >= 50 then
                match natState with
                | Some message ->
                    packetQueues.[0].Enqueue(message)
                    yield (255, 0, message) 
                | None -> ()
                yield! loop ((i + 1) % 50) natState 0
            else
                match programStates.[i] with
                | WaitingForInput _ ->
                    if packetQueues.[i].Count = 0 then
                        programStates.[i] <- expectInput -1L programStates.[i]
                        yield! loop ((i + 1) % 50) natState (nIdleComputers + 1)
                    else
                        let x, y = packetQueues.[i].Dequeue()
                        programStates.[i] <-
                            programStates.[i]
                            |> expectInput x
                            |> expectInput y
                        yield! loop ((i + 1) % 50) natState 0
                | OutputtingValue _ ->
                    let address, state1 = expectOutput programStates.[i]
                    let x, state2 = expectOutput state1
                    let y, newProgramState = expectOutput state2
                    programStates.[i] <- newProgramState
                    yield (i, int address, (x, y))
                    if address = 255L then
                        yield! loop ((i + 1) % 50) (Some (x, y)) 0
                    else
                        packetQueues.[int address].Enqueue(x, y)
                        programStates.[i] <- newProgramState
                        yield! loop ((i + 1) % 50) natState 0
                | Halted _ ->
                    failwithf "%i halted" i
        }
    loop 0 None 0

let result1 =
    runNetwork ()
    |> Seq.pick (fun (_, address, (x, y)) ->
        if address = 255 then Some y else None)

let result2 =
    let yValues = HashSet<int64>()
    runNetwork ()
    |> Seq.choose (fun (fromAddress, toAddress, message) ->
        if fromAddress = 255 && toAddress = 0 then Some (snd message) else None)
    |> Seq.find (fun y ->
        if yValues.Contains(y) then
            true
        else
            yValues.Add(y) |> ignore
            false)

// 13471 is too high