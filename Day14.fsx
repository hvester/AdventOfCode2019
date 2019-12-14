open System.IO
open System.Collections.Generic

let input = File.ReadAllText("data/input14.txt")

let parseAmountAndChemical (str : string) =
    match str.Trim().Split([|' '|]) with
    | [| amount; chemical |] -> (int amount, chemical)
    | _ -> failwithf "Invalid string: %s" str

let parseFormulas (str : string) =
    str.Split([|'\n'|])
    |> Array.filter (fun s -> s.Trim().Length > 0)
    |> Array.map (fun line ->
        let formulaSides = line.Split([|'='|])
        let inputChemicals = 
            formulaSides.[0].Split([|','|])
            |> Array.map parseAmountAndChemical
        let outputChemical =
            formulaSides.[1].Substring(2)
            |> parseAmountAndChemical
        (inputChemicals, outputChemical) )

let rec orderFormulas formulas =
    seq {
        let allInputChemicals =
            formulas
            |> Array.collect (fun (inputs, _) -> inputs |> Array.map snd)
            |> Array.distinct
        let appearInInputs, notInInputs =
            formulas
            |> Array.partition (fun (_, (_, outputChemical)) ->
                Array.contains outputChemical allInputChemicals)
        yield! notInInputs
        yield! orderFormulas appearInInputs
    }

let getAmount (d : Dictionary<string, int>) chemical =
    match d.TryGetValue chemical with
    | false, _ -> 0
    | true, amount -> amount

let addAmount (d : Dictionary<string, int>) chemical amount =
    d.[chemical] <- (getAmount d chemical) + amount

let updateNeededAndExtra needed extra formula =
    let inputs, (outputAmount, outputChemical) = formula
    let currentAmount = getAmount extra outputChemical
    let neededAmount = getAmount needed outputChemical
    let factor =
        if (neededAmount - currentAmount) % outputAmount = 0 then
            (neededAmount - currentAmount) / outputAmount
        else
            (neededAmount - currentAmount) / outputAmount + 1
    needed.Remove(outputChemical) |> ignore
    extra.[outputChemical] <- outputAmount * factor - neededAmount
    for inputAmount, inputChemical in inputs do
        addAmount needed inputChemical (inputAmount * factor)

let calculateNeededOre formulas =
    let orderedFormulas = orderFormulas formulas
    let needed = Dictionary<string, int>()
    needed.Add("FUEL", 1)
    let extra = Dictionary<string, int>()
    while not (needed.Count = 1 && needed.ContainsKey("ORE")) do
        orderedFormulas
        |> Seq.find (fun (_, (_, outputChemical)) ->
            needed.ContainsKey(outputChemical) )
        |> updateNeededAndExtra needed extra
    needed.["ORE"]

let formulas = parseFormulas input

let result1 = calculateNeededOre formulas