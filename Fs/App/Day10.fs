module Day10

(*
(0), 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, (22)    skip none       2 choose 0 * 1 choose 0
(0), 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, (22)        skip 11         2 choose 0 * 1 choose 1
(0), 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, (22)       skip 5          2 choose 1 * 1 choose 0
(0), 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, (22)       skip 6          2 choose 1 * 1 choose 0
(0), 1, 4, 6, 7, 10, 12, 15, 16, 19, (22)           skip 5, 11      2 choose 1 * 1 choose 1
(0), 1, 4, 5, 7, 10, 12, 15, 16, 19, (22)           skip 6, 11      2 choose 1 * 1 choose 1
(0), 1, 4, 7, 10, 11, 12, 15, 16, 19, (22)          skip 5, 6       2 choose 2 * 1 choose 0
(0), 1, 4, 7, 10, 12, 15, 16, 19, (22)              skip 5, 6, 11   2 choose 2 * 1 choose 1
*)

let sample1 = [|
    "16"
    "10"
    "15"
    "5"
    "1"
    "11"
    "7"
    "19"
    "6"
    "12"
    "4"
|]

let sample2 = [|
    "28"
    "33"
    "18"
    "42"
    "31"
    "14"
    "46"
    "20"
    "48"
    "47"
    "24"
    "23"
    "49"
    "45"
    "19"
    "38"
    "39"
    "11"
    "1"
    "32"
    "25"
    "35"
    "8"
    "17"
    "7"
    "9"
    "4"
    "2"
    "34"
    "10"
    "3"
|]

let productOf joltDifferences =
    let ones = joltDifferences |> Array.find (fun (k, _) -> k = 1)
    let threes = joltDifferences |> Array.find (fun (k, _) -> k = 3)
    (snd ones) * (snd threes)

let toJoltageDifferences (data: string []) =
    let sorted = data |> Array.map int |> Array.sort
    let adapters = [|(Array.last sorted) + 3|] |> Array.append sorted
    let outlets = sorted |> Array.append [|0|]
    outlets |> Array.map2 (-) adapters

let solve1 data =
    data
    |> toJoltageDifferences
    |> Array.groupBy id
    |> Array.map (fun (k, v) -> k, v |> Array.length)
    |> productOf

let rec factorial = function
    | x when x = 0 -> 1
    | x -> x*factorial(x-1)

let choose n k =
    (n |> factorial) / ((k |> factorial) * ((n - k) |> factorial))

let sumOfChoices n =
    let maxToChoose = if n > 2 then 2 else n
    [0..maxToChoose] |> List.sumBy (choose n)

let solve2 data =
    data
    |> toJoltageDifferences
    |> Array.map string
    |> Array.reduce (fun x y -> sprintf "%s%s" x y)
    |> fun s -> s.Split('3')
    |> Array.filter (fun x -> x.Length > 1)
    |> Array.map ((fun x -> sumOfChoices (x.Length - 1)) >> int64)
    |> Array.reduce (*)
