module Batch
open System.Text

type AccumulatorType =
| StringAccumulator of StringBuilder
| ArrayAccumulator of string array

type Accumulator = {
    Accumulated: string array
    Builder: AccumulatorType
}

let accumulate = function
    | { Accumulated = acc; Builder = StringAccumulator b } ->
        let accumulated = [|b.ToString()|] |> Array.append acc
        { Accumulated = accumulated; Builder = StringAccumulator (StringBuilder()) }
    | { Accumulated = acc; Builder = ArrayAccumulator b } ->
        let accumulated = b |> Array.append acc
        { Accumulated = accumulated; Builder = ArrayAccumulator [||] }

let build appendString accumulator line =
    let appendLine = sprintf "%s%s" line appendString
    match accumulator with
    | StringAccumulator b ->
        StringAccumulator (b.Append(appendLine))
    | ArrayAccumulator b ->
        ArrayAccumulator ([|appendLine|] |> Array.append b)

let consolidate separator appendString batchAccumulator line =
    if line = separator then
        accumulate batchAccumulator
    else
        { batchAccumulator with Builder = (build appendString batchAccumulator.Builder line) }

let consolidateData accumulator separator appendString data =
    [|separator|]
    |> Array.append data
    |> Array.fold (consolidate separator appendString) accumulator
    |> fun a -> a.Accumulated

let consolidateToStringsByBlankLine = consolidateData { Accumulated = [||]; Builder = StringAccumulator (StringBuilder()) } ""
let consolidateToStringArrByBlankLine = consolidateData { Accumulated = [||]; Builder = ArrayAccumulator [||] } ""
