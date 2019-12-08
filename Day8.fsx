open System.IO

let imageData =
    File.ReadAllText("data/input8.txt").Trim().ToCharArray()
    |> Array.map (string >> int)


let countBy pred arr =
    (0, arr)
    ||> Array.fold (fun acc x ->
        if pred x then acc + 1 else acc)


let result1 =
    let selectedLayer =
        imageData
        |> Array.chunkBySize (25 * 6)
        |> Array.minBy (countBy ((=) 0))
    let ones = countBy ((=) 1) selectedLayer
    let twos = countBy ((=) 2) selectedLayer
    ones * twos


    