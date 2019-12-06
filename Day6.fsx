open System.IO
open System.Collections.Generic

let input = File.ReadAllLines("data/input6.txt")

let relationships =
    input
    |> Array.map (fun s ->
        let parts = s.Split([|')'|])
        (parts.[0], parts.[1]) )

let orbitersDict =
    relationships
    |> Array.groupBy fst
    |> Array.map (fun (orbitee, xs) -> (orbitee, xs |> Array.map snd))
    |> dict

let result1 =
    let rec recurse depth orbitee =
        let orbiters =
            match orbitersDict.TryGetValue orbitee with
            | false, _ -> [||]
            | true, xs -> xs
        orbiters
        |> Array.sumBy (recurse (depth + 1))
        |> ((+) depth)
    recurse 0 "COM"

let orbiteeDict =
    relationships
    |> Array.map (fun (orbitee, orbiter) -> (orbiter, orbitee))
    |> dict

let getOrbiteeChain orbiter =
    let rec loop acc x =
        if x = "COM" then
            acc
        else
            let orbitee = orbiteeDict.[x]
            loop (orbitee :: acc) orbitee
    loop [] orbiter

let rec skipCommon xs ys =
    match xs, ys with
    | x :: xtail, y :: ytail when x = y -> skipCommon xtail ytail
    | _ -> xs, ys

let result2 =
    let distinctParts = skipCommon (getOrbiteeChain "YOU") (getOrbiteeChain "SAN")
    (fst distinctParts).Length + (snd distinctParts).Length
