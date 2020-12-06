module Batch

type Accumulator = {
    Accumulator: string array seq
    Builder: string array
}

let accumulate separator accumulator line =
    if line = separator then
        { Accumulator = [|accumulator.Builder|] |> Seq.append accumulator.Accumulator; Builder = [||] }
    else
        { accumulator with Builder = [|line|] |> Array.append accumulator.Builder}

let consolidateData accumulator separator data =
    [|separator|]
    |> Array.append data
    |> Array.fold (accumulate separator) accumulator
    |> fun a -> a.Accumulator

let consolidate = consolidateData { Accumulator = seq [||]; Builder = [||]} ""
