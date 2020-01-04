open System.IO

let mazeStr = File.ReadAllText("data/input20.txt")

let (|Letter|_|) (c : char) = if c >= 'A' && c <= 'Z' then Some c else None

let getMazeEdges chars =
    let dotPositions =
        Map.toSeq chars
        |> Seq.choose (fun (position, c) ->
            if c = '.' then Some position else None)
    let is = Seq.map fst dotPositions
    let js = Seq.map snd dotPositions
    (Seq.min is, Seq.max is, Seq.min js, Seq.max js)

let parseMaze (str : string) =
    let chars =
        str.Split([|'\n'|])
        |> Array.indexed
        |> Array.collect (fun (j, line) ->
            line.ToCharArray()
            |> Array.indexed
            |> Array.map (fun (i, c) -> ((i, j), c) ))
        |> Map.ofArray
    let teleports =
        Map.toSeq chars
        |> Seq.filter (fun (_, c) -> c = '.')
        |> Seq.choose (fun ((i, j), c) ->
            seq {
                yield ((i - 2, j), (i - 1, j))
                yield ((i + 1, j), (i + 2, j))
                yield ((i, j - 2), (i, j - 1))
                yield ((i, j + 1), (i, j + 2))
            }
            |> Seq.tryPick (fun (position1, position2) ->
                match Map.tryFind position1 chars, Map.tryFind position2 chars with
                | Some (Letter c1), Some (Letter c2) ->
                    Some ((c1, c2), (i, j))
                | _ -> None))
    let findTeleport letters = teleports |> Seq.find (fst >> ((=) letters)) |> snd
    let startPosition = findTeleport ('A', 'A')
    let endPosition = findTeleport ('Z', 'Z')
    let minDotI, maxDotI, minDotJ, maxDotJ = getMazeEdges chars
    let teleportConnections =
        teleports
        |> Seq.groupBy fst
        |> Seq.collect (fun (_, connectedTeleports) ->
            match Seq.toList connectedTeleports with
            | [(_, position1); (_, position2) ] ->
                [ (position1, position2); (position2, position1) ]
            | _ ->
                [])
        |> Seq.map (fun (fromPosition, toPosition) ->
            let i, j = fromPosition
            let inward = i > minDotI && i < maxDotI && j > minDotJ && j < maxDotJ
            (fromPosition, (toPosition, inward)) )
        |> Map.ofSeq
    (chars, teleportConnections, startPosition, endPosition)

let chars, teleportConnections, startPosition, endPosition =
    parseMaze mazeStr

let findShortestPath next start goal =
    let rec loop steps visited nodes =
        if List.contains goal nodes then
            Some steps
        elif List.isEmpty nodes then
            None
        else
            let newNodes =
                nodes
                |> List.collect next
                |> List.distinct
                |> List.filter (fun node -> not (Set.contains node visited))
            let newVisited =
                Set.union visited (Set.ofList newNodes)
            loop (steps + 1) newVisited newNodes
    loop 0 Set.empty [ start ]

let result1 =
    let next (i, j) =
        let neighbourPositions =
            [ (i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1) ]
            |> List.filter (fun position ->
                Map.tryFind position chars = Some '.')
        match Map.tryFind (i, j) teleportConnections with
        | Some (teleportPosition, _) -> teleportPosition :: neighbourPositions
        | None -> neighbourPositions
    findShortestPath next startPosition endPosition
    |> Option.get

let result2 =
    let next (i, j, level) =
        let neighbourPositions =
            [ (i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1) ]
            |> List.filter (fun position ->
                Map.tryFind position chars = Some '.')
            |> List.map (fun (i, j) -> (i, j, level))
        match Map.tryFind (i, j) teleportConnections with
        | Some ((toI, toJ), inwardTeleport) ->
            if inwardTeleport then
                (toI, toJ, level + 1) :: neighbourPositions
            elif not inwardTeleport && level > 0 then
                (toI, toJ, level - 1) :: neighbourPositions
            else
                neighbourPositions
        | None ->
            neighbourPositions
    let startI, startJ = startPosition
    let endI, endJ = endPosition
    findShortestPath next (startI, startJ, 0) (endI, endJ, 0)
    |> Option.get
