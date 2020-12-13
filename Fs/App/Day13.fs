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

let busIdsWithConstraintFrom (data: string []) =
    data.[1].Split(',')
    |> Array.indexed
    |> Array.filter (snd >> (<>) "x")
    |> Array.map (fun (i,b) -> ((uint64)i,(uint64)b))
    |> Array.sortByDescending snd

let toModEquations (offset, bus) = ((bus - (offset % bus)) % bus, bus)

let rec candidates (current: uint64, increment: uint64) = seq {
    yield current
    yield! candidates (current+increment,increment)
}

let nextSolution (current: uint64, increment: uint64) (equals, modThis) =
    candidates (current, increment)
    |> Seq.find (fun x -> x % modThis = equals)
    |> fun x -> (x, increment * modThis)

let solve2 data =
    busIdsWithConstraintFrom data
    |> Array.map toModEquations
    |> Array.reduce nextSolution
    |> fst


let x = [|
    "939"
    "17,x,13,19"
|]

(*
[|(48UL, 613UL); (17UL, 367UL); (7UL, 41UL); (11UL, 37UL); (46UL, 29UL);
    (40UL, 23UL); (36UL, 19UL); (0UL, 17UL); (61UL, 13UL)|]
t % 613 = (613-48)
t % 29 = 46 -> take mod then subtract from mod
*)
