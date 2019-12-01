open System.IO

let moduleMasses =
    File.ReadAllLines("data/input1.txt")
    |> Array.map int

let getRequiredFuel mass = mass / 3 - 2

let result1 = moduleMasses |> Array.sumBy getRequiredFuel

let getRequiredTotalFuel moduleMass =
    moduleMass
    |> Seq.unfold (fun mass ->
        let additionalFuel = getRequiredFuel mass
        if additionalFuel > 0 then
            Some (additionalFuel, additionalFuel)
        else
            None)
    |> Seq.sum

let result2 = moduleMasses |> Array.sumBy getRequiredTotalFuel
