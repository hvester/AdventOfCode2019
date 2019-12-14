open System.IO
open System.Collections.Generic

let input = File.ReadAllText("data/input14.txt")

let parseAmountAndChemical (str : string) =
    match str.Trim().Split([|' '|]) with
    | [| amount; chemical |] -> (int64 amount, chemical)
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

let getAmount (d : Dictionary<string, int64>) chemical =
    match d.TryGetValue chemical with
    | false, _ -> 0L
    | true, amount -> amount

let addAmount (d : Dictionary<string, int64>) chemical amount =
    d.[chemical] <- (getAmount d chemical) + amount

let updateNeededAndExtra needed extra formula =
    let inputs, (outputAmount, outputChemical) = formula
    let currentAmount = getAmount extra outputChemical
    let neededAmount = getAmount needed outputChemical
    let factor =
        if (neededAmount - currentAmount) % outputAmount = 0L then
            (neededAmount - currentAmount) / outputAmount
        else
            (neededAmount - currentAmount) / outputAmount + 1L
    needed.Remove(outputChemical) |> ignore
    extra.[outputChemical] <- outputAmount * factor - neededAmount
    for inputAmount, inputChemical in inputs do
        addAmount needed inputChemical (inputAmount * factor)

let calculateNeededOre orderedFormulas fuelAmount =
    let needed = Dictionary<string, int64>()
    needed.Add("FUEL", fuelAmount)
    let extra = Dictionary<string, int64>()
    while not (needed.Count = 1 && needed.ContainsKey("ORE")) do
        orderedFormulas
        |> Seq.find (fun (_, (_, outputChemical)) ->
            needed.ContainsKey(outputChemical) )
        |> updateNeededAndExtra needed extra
    needed.["ORE"]

let orderedFormulas = 
    parseFormulas input
    |> orderFormulas

let result1 = calculateNeededOre orderedFormulas 1L

let result2 =
    let f = calculateNeededOre orderedFormulas
    let y = 1000000000000L
    // Find largest x so that f(x) <= y
    let initialUpperBound =
        Seq.initInfinite (fun i -> pown 2L i)
        |> Seq.find (fun x -> f x > y)
    let rec loop lb ub =
        if ub - lb <= 1L then
            lb
        else
            let mid = (lb + ub) / 2L
            let fmid = f mid
            if fmid = y then
                mid
            elif fmid > y then
                loop lb mid
            else
                loop mid ub
    loop (initialUpperBound / 2L) initialUpperBound