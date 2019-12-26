open System.Collections.Generic

let input =
    """
    ##...
    #.###
    .#.#.
    #....
    ..###
    """

let parseInput (str : string) =
    str.Trim().Split([|'\n'|])
    |> Array.map (fun line ->
        line.Trim().ToCharArray()
        |> Array.map ((=) '#'))

let initialBugs = parseInput input

let stateToInt (chars : bool array array) =
    (0, Array.concat chars)
    ||> Array.fold (fun x hasBug -> if hasBug then 2 * x + 1 else 2 * x)

let findFirstRepeatingState initialState =
    let n = initialState |> Array.head |> Array.length
    let m = initialState |> Array.length
    let visitedStates = HashSet<int>()
    let rec loop (state : bool array array) =
        let count i j =
            if i >= 0 && i < n && j >= 0 && j < m then
                if state.[j].[i] then 1 else 0
            else
                0
        let stateInt = stateToInt state
        if visitedStates.Contains(stateInt) then
            state
        else
            visitedStates.Add(stateToInt state) |> ignore
            let newState =
                state
                |> Array.mapi (fun j row ->
                    row
                    |> Array.mapi (fun i hasBug ->
                        let neighbourCount =
                            count (i - 1) j +
                            count (i + 1) j +
                            count i (j - 1) +
                            count i (j + 1)
                        if hasBug then
                            neighbourCount = 1
                        else
                            neighbourCount = 1 || neighbourCount = 2))
            loop newState
    loop initialState

let calculateBiodiversity (state : bool array array) =
    Array.concat state
    |> Array.indexed
    |> Array.sumBy (fun (i, hasBug) ->
        if hasBug then
            pown 2 i
        else
            0)

let result1 =
    initialBugs
    |> findFirstRepeatingState
    |> calculateBiodiversity


let getNeighbours (depth, i, j) =
    seq {
        if i = 0 then yield (depth + 1, 1, 2)
        if i = 4 then yield (depth + 1, 3, 2)
        if j = 0 then yield (depth + 1, 2, 1)
        if j = 4 then yield (depth + 1, 2, 3)

        if i = 1 && j = 2 then for k in 0 .. 4 do yield (depth - 1, 0, k)
        if i = 3 && j = 2 then for k in 0 .. 4 do yield (depth - 1, 4, k)
        if i = 2 && j = 1 then for k in 0 .. 4 do yield (depth - 1, k, 0)
        if i = 2 && j = 3 then for k in 0 .. 4 do yield (depth - 1, k, 4)

        if i > 0 then yield (depth, i - 1, j)
        if i < 4 then yield (depth, i + 1, j)
        if j > 0 then yield (depth, i, j - 1)
        if j < 4 then yield (depth, i, j + 1)
    }
    |> Seq.filter (fun (_, i, j) -> i <> 2 || j <> 2)

let simulate steps initialState =
    let rec loop stepsLeft (state : Set<int * int * int>) =
        if stepsLeft > 0 then
            let newState =
                state
                |> Seq.collect getNeighbours
                |> Seq.distinct
                |> Seq.filter (fun position ->
                    let nNeighbourBugs =
                        getNeighbours position
                        |> Seq.sumBy (fun neighbourPosition ->
                            if Set.contains neighbourPosition state then 1 else 0)
                    if Set.contains position state then
                        nNeighbourBugs = 1
                    else
                        nNeighbourBugs = 1 || nNeighbourBugs = 2)
                |> Set.ofSeq
            loop (stepsLeft - 1) newState
        else
            state
    loop steps initialState

let result2 =
    let initialState =
        seq {
            for i in 0 .. 4 do
                for j in 0 .. 4 do
                    if initialBugs.[i].[j] then
                        yield (0, i, j)
        }
        |> Set.ofSeq
    simulate 200 initialState
    |> Set.count