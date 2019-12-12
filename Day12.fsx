open System.IO
open System.Text.RegularExpressions

let inputStr = File.ReadAllText("data/input12.txt")

let parseMoonPositions (str : string) =
    let regex = Regex(@"<x=(-?\d+),\s*y=(-?\d+),\s*z=(-?\d+)>")
    [ for m in regex.Matches(str) do
        yield (int m.Groups.[1].Value, int m.Groups.[2].Value, int m.Groups.[3].Value) ]

let initialPositions = parseMoonPositions inputStr

let calculateNewVelocities positions velocities =
    let getNewV v t0 t = if t > t0 then v + 1 elif t < t0 then v - 1 else v
    (positions, velocities)
    ||> List.map2 (fun (x0, y0, z0) velocity ->
        (velocity, positions)
        ||> List.fold (fun (vx, vy, vz) (x, y, z) ->
            (getNewV vx x0 x, getNewV vy y0 y, getNewV vz z0 z) ))

let calculateNewPositions positions velocities =
    (positions, velocities)
    ||> List.map2 (fun (x, y, z) (vx, vy, vz) ->
        (x + vx, y + vy, z + vz) )

let simulate nSteps initialPositions =
    let rec loop stepsLeft positions velocities =
        if stepsLeft <= 0 then
            (positions, velocities)
        else
            let newVelocities = calculateNewVelocities positions velocities
            let newPositions = calculateNewPositions positions newVelocities
            loop (stepsLeft - 1) newPositions newVelocities
    let initialVelocities = [ for _ in 1 .. (List.length initialPositions) do yield (0, 0, 0)]
    loop nSteps initialPositions initialVelocities

let getEnergy (positions, velocities) =
    (positions, velocities)
    ||> List.map2 (fun (x, y, z) (vx, vy, vz) ->
        (abs x + abs y + abs z) * (abs vx + abs vy + abs vz) )
    |> List.sum

let result1 =
    simulate 1000 initialPositions
    |> getEnergy
