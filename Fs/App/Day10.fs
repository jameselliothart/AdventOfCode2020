module Day10

(*
(0), 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, (22)
(0), 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, (22)       skip 5
(0), 1, 4, 7, 10, 11, 12, 15, 16, 19, (22)          skip 5, 6
(0), 1, 4, 7, 10, 12, 15, 16, 19, (22)              skip 5, 6, 11
(0), 1, 4, 6, 7, 10, 12, 15, 16, 19, (22)           skip 5, 11
(0), 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, (22)       skip 6
(0), 1, 4, 5, 7, 10, 12, 15, 16, 19, (22)           skip 6, 11
(0), 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, (22)        skip 11
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

let toAdaptersAndOutlets data =
    let sorted = data |> Array.map int |> Array.sort
    let adapters = [|(Array.last sorted) + 3|] |> Array.append sorted
    let outlets = sorted |> Array.append [|0|]
    adapters, outlets

let solve1 (data: string []) =
    let adapters, outlets = data |> toAdaptersAndOutlets
    outlets
    |> Array.map2 (-) adapters
    |> Array.groupBy id
    |> Array.map (fun (k, v) -> k, v |> Array.length)
    |> productOf
