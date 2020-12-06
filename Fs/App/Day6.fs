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
    |> consolidateByBlankLine ""
    |> Array.sumBy (Seq.distinct >> Seq.length)

sample |> totalAffirmative |> printfn "%A"
