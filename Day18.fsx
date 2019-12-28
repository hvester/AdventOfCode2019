open System
open System.IO
open System.Collections.Generic

let input = File.ReadAllText("data/input18.txt")

let parseMaze (str : string) =
    let lines =
        str.Trim().Split('\n')
        |> Array.map (fun line -> line.Trim())
    let n = lines.[0].Length
    let m = lines.Length
    Array2D.init n m (fun i j -> lines.[j].[i])

let mazeToSeq maze =
    seq {
        for i in 0 .. Array2D.length1 maze - 1 do
            for j in 0 .. Array2D.length2 maze - 1 do
                yield (i, j, maze.[i, j])
    }

let findStartPosition maze =
    mazeToSeq maze
    |> Seq.pick (fun (i, j, c) -> if c = '@' then Some (i, j) else None)

let isKey c = c >= 'a' && c <= 'z'
let isDoor c = c >= 'A' && c <= 'Z'
let requiredKey (door : char) = char (int door + 32)

let countKeys maze =
    mazeToSeq maze
    |> Seq.filter (fun (_, _, c) -> isKey c)
    |> Seq.length

let findShortestPath maze =
    let keyCount = countKeys maze
    let visited = HashSet<Set<char> * int * int>()
    let rec loop steps states =
        if states |> List.exists (fun (keys, _, _) -> Set.count keys = keyCount) then
            steps
        else
            let newStates =
                states
                |> List.collect (fun (keys, i, j) ->
                    [ (i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1) ]
                    |> List.choose (fun (ii, jj) ->
                        match maze.[ii, jj] with
                        | '#' -> None
                        | '.' | '@' -> Some (keys, ii, jj)
                        | k when isKey k ->
                            Some (Set.add k keys, ii, jj)
                        | d when isDoor d ->
                            if Set.contains (requiredKey d) keys then
                                Some (keys, ii, jj)
                            else
                                None
                        | _ ->
                            failwith "Invalid character"))
                    |> List.distinct
                    |> List.filter (visited.Contains >> not)
            for state in newStates do visited.Add(state) |> ignore
            loop (steps + 1) newStates
    let i, j = findStartPosition maze 
    loop 0 [ (Set.empty, i, j) ]

let maze = parseMaze input

let result1 = findShortestPath maze
