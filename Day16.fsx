open System
open System.IO

let input = File.ReadAllText("data/input16.txt")

let parseNumbers (str : string) = str.Trim().ToCharArray() |> Array.map (string >> int)

let inputNumbers = parseNumbers input

let basePattern = [| 0; 1; 0; -1 |]

let getMultiplier outputIndex numberIndex =
    basePattern.[((numberIndex + 1) / (outputIndex + 1)) % basePattern.Length]

let getLastDigit number = abs number % 10

let phase numbers =
    Array.init (Array.length numbers) (fun outputIndex ->
        numbers
        |> Array.indexed
        |> Array.sumBy (fun (i, x) ->
            getMultiplier outputIndex i * x)
        |> getLastDigit)

let rec fft nPhases numbers =
    printfn "%i" nPhases
    if nPhases <= 0 then
        numbers
    else
        fft (nPhases - 1) (phase numbers)

let result1 =
    let finalNumbers = inputNumbers
    String.Join("", Array.take 8 finalNumbers) |> int
