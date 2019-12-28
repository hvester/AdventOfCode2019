open System.IO

let input = File.ReadAllText("data/input22.txt")

type Shuffle =
    | NewStack
    | Cut of int
    | Increment of int

let (|StartsWith|_|) (startString : string) (str : string) =
    if str.Length >= startString.Length && str.Substring(0, startString.Length) = startString then
        Some (str.Substring(startString.Length))
    else
        None

let parseShuffles (str : string) =
    str.Trim().Split('\n')
    |> Array.map (fun line ->
        match line.Trim() with
        | StartsWith "deal into new stack" _ ->
            NewStack
        | StartsWith "cut" numberString ->
            Cut (int (numberString.Trim()))
        | StartsWith "deal with increment" numberString ->
            Increment (int (numberString.Trim()))
        | _ ->
            failwith "Invalid line")

let applyShuffle deck shuffle =
    if Array.sort deck <> [| 0 .. deck.Length - 1 |] then
        failwith "növcn"
    match shuffle with
    | NewStack ->
        Array.rev deck
    | Cut n ->
        deck
        |> Array.permute (fun i ->
            let j = (i - n) % deck.Length
            if j < 0 then j + deck.Length else j)
    | Increment n ->
        deck
        |> Array.permute (fun i ->
            i * n % deck.Length)

let shuffleDeck deck shuffles =
    (deck, shuffles)
    ||> Array.fold applyShuffle    

let shuffles = parseShuffles input

let result1 =    
    shuffleDeck [| 0 .. 10006 |] shuffles
    |> Array.indexed
    |> Array.pick (fun (position, card) ->
        if card = 2019 then Some position else None)
