module Day6
open Batch
open System.Text

let sample = [|
    "abc"
    ""
    "a"
    "b"
    "c"
    ""
    "ab"
    "ac"
    ""
    "a"
    "a"
    "a"
    "a"
    ""
    "b"
|]

let totalAffirmative data =
    data
    |> consolidate
    |> Seq.map (fun x -> x |> Array.reduce (+))
    |> Seq.sumBy (Seq.distinct >> Seq.length)

let totalUnanimousAffirmative data =
    data
    |> consolidate
    |> Seq.sumBy (Seq.map set >> Set.intersectMany >> Seq.length)

sample |> totalAffirmative |> printfn "%A"
sample |> totalUnanimousAffirmative |> printfn "%A"
