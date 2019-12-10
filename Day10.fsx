open System
open System.IO

let input = File.ReadAllText("data/input10.txt") 

let rec gcd a b =
    if b = 0 then
        a
    else
        gcd b (a % b)

let parseAsteroidPositions (str : string) =
    [ for y, row in Array.indexed (str.Split([|'\n'|])) do
        for x, c in Array.indexed (row.ToCharArray()) do
            if c = '#' then
                yield (x, y) ]

let findBestAsteroid asteroids =
    asteroids
    |> List.map (fun (x0, y0) ->
        let linesOfSight =
            asteroids
            |> List.map (fun (x, y) -> (x0 - x, y0 - y) )
            |> List.filter (fun dxdy -> dxdy <> (0, 0))
            |> List.groupBy (fun (dx, dy) ->
                let d = gcd (abs dx) (abs dy)
                (dx / d, dy / d))
            |> List.length
        (x0, y0), linesOfSight)
    |> List.maxBy snd

let orderAsteroids (x0, y0) asteroids =
    asteroids
    |> List.map (fun (x, y) -> ((x, y), (x - x0, y - y0)) )
    |> List.filter (fun (_, dxdy) -> dxdy <> (0, 0))
    |> List.groupBy (fun (_, (dx, dy)) ->
        let d = gcd (abs dx) (abs dy)
        (dx / d, dy / d))
    |> List.collect (fun (_, asteroidsInLines) ->
        asteroidsInLines
        |> List.sortBy (fun (_, (dx, dy)) -> abs dx + abs dy)
        |> List.mapi (fun i (coordinates, (dx, dy)) ->
            let direction = (atan2 (float dx) (float -dy) + 2.0 * Math.PI) % (2.0 * Math.PI)
            ((i, direction), coordinates) ))
    |> List.sortBy fst

let asteroids = parseAsteroidPositions input

let bestAsteroid, result1 = findBestAsteroid asteroids

let asteroid200 =
    orderAsteroids bestAsteroid asteroids
    |> List.item 199
    |> snd

let result2 = 100 * (fst asteroid200) + (snd asteroid200)
