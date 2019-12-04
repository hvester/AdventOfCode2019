let inputRange = [ 138241 .. 674034 ]

let getDigits number =
    let rec loop acc x =
        if x <= 0 then
            acc
        else
            loop (x % 10 :: acc) (x / 10)
    loop [] number

let isIncreasing digits =
    digits
    |> List.pairwise
    |> List.forall (fun (x, y) -> x <= y)

let hasSameAdjacentDigits digits =
    digits
    |> List.pairwise
    |> List.exists (fun (x, y) -> x = y)

let result1 =
    inputRange
    |> List.filter (fun number ->
        let digits = getDigits number
        isIncreasing digits && hasSameAdjacentDigits digits)
    |> List.length

let hasExactlyTwoAdjacentDigits digits =
    let rec loop remaining =
        match remaining with
        | [] -> false
        | [ _ ] -> false
        | [ x; y ] -> x = y
        | x :: y :: z :: xs ->
            if x = y then
                if y <> z then
                    true
                else
                    loop (List.skipWhile ((=) x) remaining)
            else
                loop (y :: z :: xs)
    loop digits

let result2 =
    inputRange
    |> List.filter (fun number ->
        let digits = getDigits number
        isIncreasing digits && hasExactlyTwoAdjacentDigits digits)
    |> List.length