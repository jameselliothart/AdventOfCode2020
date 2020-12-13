module Day13

let data = System.IO.File.ReadAllLines "./Data/Day13.txt"

let sample = [|
    "939"
    "7,13,x,x,59,x,31,19"
|]

let departTimeFrom (data: string []) =
    (int)data.[0]

let busIdsFrom (data: string []) =
    data.[1].Split(',')
    |> Array.filter ((<>) "x")
    |> Array.map int

let nearestTimeNotBefore time interval =
    let lb = time / interval
    match interval * lb with
    | t when t < time -> interval * (lb + 1)
    | t -> t

let solve data =
    let time, busIds = departTimeFrom data, busIdsFrom data
    busIds
    |> Array.map (fun i -> (i, nearestTimeNotBefore time i))
    |> Array.sortBy snd
    |> Array.head
    |> fun (i, t) -> i * (t - time)

solve data
