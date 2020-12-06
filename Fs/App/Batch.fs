module Batch
open System.Text

type Accumulator =
| StringAccumulator of Accumulated: string array * Builder : StringBuilder
| ArrayAccumulator of Accumulated: string array array * Builder : string array

// type Accumulator = {
//     Accumulated: string array
//     Builder: AccumulatorType
// }

let accumulate = function
    | StringAccumulator (acc, b) ->
        let accumulated = [|b.ToString()|] |> Array.append acc
        StringAccumulator (accumulated, StringBuilder())
    | ArrayAccumulator (acc, b) ->
        let accumulated = [|b|] |> Array.append acc
        ArrayAccumulator (accumulated, [||])

let build appendString accumulator line =
    let appendLine = sprintf "%s%s" line appendString
    match accumulator with
    | StringAccumulator (acc, b) ->
        StringAccumulator (acc, b.Append(appendLine))
    | ArrayAccumulator (acc, b) ->
        ArrayAccumulator (acc, [|appendLine|] |> Array.append b)

let consolidate separator appendString batchAccumulator line =
    if line = separator then
        accumulate batchAccumulator
    else
        build appendString batchAccumulator line

let consolidateData accumulator separator appendString data =
    [|separator|]
    |> Array.append data
    |> Array.fold (consolidate separator appendString) accumulator

let consolidateToStringsByBlankLine = consolidateData (StringAccumulator ( [||], StringBuilder() )) ""
let consolidateToStringArrByBlankLine = consolidateData (ArrayAccumulator ( [||], [||] )) ""
