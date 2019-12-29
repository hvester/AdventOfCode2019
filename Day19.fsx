#load "Interpreter.fsx"

open System.IO
open Interpreter

let inputProgram = File.ReadAllText("data/input19.txt").Split([|','|]) |> Array.map int64

let isPulledFrom x y =
    startProgram inputProgram
    |> expectInput (int64 x)
    |> expectInput (int64 y)
    |> expectOutput
    |> fst
    |> ((=) 1L)

let result1 =
    seq {
        for x in 0 .. 49 do
            for y in 0 .. 49 do
                yield if isPulledFrom x y then 1 else 0
    }
    |> Seq.sum

let result2 =
    let x, y =
        Seq.initInfinite (fun i -> (i, 99))
        |> Seq.find (fun (x, y) -> isPulledFrom x y)
        |> Seq.unfold (fun (x, y) ->
            if isPulledFrom x (y + 1) then
                Some ((x, y), (x, y + 1))
            elif isPulledFrom (x + 1) (y + 1) then
                Some ((x, y), (x + 1, y + 1))
            elif isPulledFrom (x + 1) y then
                Some ((x, y), (x + 1, y))
            else
                failwith "There is hole in the beam")
        |> Seq.pick (fun (x, y) ->
            if isPulledFrom (x + 99) (y - 99) then
                Some (x, y - 99)
            else
                None)
    10000 * x + y
