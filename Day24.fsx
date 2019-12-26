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
    parseInput input
    |> findFirstRepeatingState
    |> calculateBiodiversity
    