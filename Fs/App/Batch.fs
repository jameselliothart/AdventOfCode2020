module Batch
open System.Text

type BatchAccumulator = {
    Accumulated: string array
    Builder: StringBuilder
}

let consolidate separator appendString batchAccumulator line =
    if line = separator then
        let accumulated = [|batchAccumulator.Builder.ToString()|] |> Array.append batchAccumulator.Accumulated
        { Accumulated = accumulated; Builder = StringBuilder() }
    else
        let appendLine = sprintf "%s%s" line appendString
        { Accumulated = batchAccumulator.Accumulated; Builder = batchAccumulator.Builder.Append(appendLine) }

let consolidateOnBlankLine = consolidate ""
