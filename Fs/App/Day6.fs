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
    [|""|]
    |> Array.append data
    |> Array.fold (consolidateOnBlankLine "") { Accumulated = [||]; Builder = StringBuilder() }
    |> fun a -> a.Accumulated
    |> Array.sumBy (Seq.distinct >> Seq.length)

sample |> totalAffirmative |> printfn "%A"
