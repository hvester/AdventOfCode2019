open System
open System.IO

let encodedImage =
    File.ReadAllText("data/input8.txt").Trim().ToCharArray()
    |> Array.map (string >> int)

let layers = encodedImage |> Array.chunkBySize (25 * 6)

let countBy pred arr =
    (0, arr)
    ||> Array.fold (fun acc x ->
        if pred x then acc + 1 else acc)

let result1 =
    let selectedLayer = layers |> Array.minBy (countBy ((=) 0))
    let ones = countBy ((=) 1) selectedLayer
    let twos = countBy ((=) 2) selectedLayer
    ones * twos

let decodedImage =
    layers
    |> Array.reduce (fun accumulatedLayer layer ->
        (accumulatedLayer, layer)
        ||> Array.map2 (fun acc x ->
            if acc = 2 then x else acc))

let renderImage image =
    let renderedRow =
        image
        |> Array.map (fun x -> if x = 0 then " " else "#")
        |> Array.chunkBySize 25
        |> Array.map (fun row ->
            sprintf "%s" (String.Join("", row)))
    String.Join("\n", renderedRow)

let result2 = renderImage decodedImage