open System.IO

let wire1, wire2 =
    let input = File.ReadAllLines("data/input3.txt")
    let wire1 = input.[0].Split([|','|])
    let wire2 = input.[1].Split([|','|])
    (wire1, wire2)
    
let parseMove (str : string) =
    let direction =
        match str.[0] with
        | 'R' -> (1, 0)
        | 'U' -> (0, 1)
        | 'L' -> (-1, 0)
        | 'D' -> (0, -1)
        | _ -> failwith "Invalid direction"
    (direction, int (str.Substring(1))) 

let getPathPositions commands =
    let steps =
        seq {
            for command in commands do
                let direction, steps = parseMove command
                for _ in 1 .. steps do
                    yield direction 
        }
    ((0, 0), steps)
    ||> Seq.scan (fun (x, y) (dx, dy) ->
        (x + dx, y + dy))

let getManhattanDistance (x, y) = abs x + abs y

let result1 =
    let positions1 = Seq.tail (getPathPositions wire1)
    let positions2 = Seq.tail (getPathPositions wire2)
    let commonPositions =
        Set.intersect (Set.ofSeq positions1) (Set.ofSeq positions2)
    commonPositions
    |> Set.toSeq
    |> Seq.map getManhattanDistance
    |> Seq.min

let result2 =
    let positions1 = Seq.tail (Seq.indexed (getPathPositions wire1))
    let positions2 = Seq.tail (Seq.indexed (getPathPositions wire2))
    let wire1Delays =
        positions1
        |> Seq.map (fun (delay, position) -> (position, delay))
        |> dict
    positions2
    |> Seq.choose (fun (delay2, position) ->
        match wire1Delays.TryGetValue position with
        | true, delay1 -> Some (delay1 + delay2)
        | false, _ -> None)
    |> Seq.min