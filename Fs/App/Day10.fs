module Day10

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

let solve1 (data: string []) =
    let sorted = data |> Array.map int |> Array.sort
    let adapters = [|(Array.last sorted) + 3|] |> Array.append sorted
    let outlets = sorted |> Array.append [|0|]
    outlets
    |> Array.map2 (-) adapters
    |> Array.groupBy id
    |> Array.map (fun (k, v) -> k, v |> Array.length)
    |> productOf
