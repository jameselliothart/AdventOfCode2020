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
    |> consolidateToStringsByBlankLine ""
    |> fun (StringAccumulator (acc, _)) -> acc
    |> Array.sumBy (Seq.distinct >> Seq.length)

let totalUnanimousAffirmative data =
    data
    |> consolidateToStringArrByBlankLine ""
    |> fun (ArrayAccumulator (acc, _)) -> acc
    |> Array.sumBy (Seq.map set >> Set.intersectMany >> Seq.length)

sample |> totalUnanimousAffirmative |> printfn "%A"
