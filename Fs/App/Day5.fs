module Day5
open System.IO

let data = File.ReadAllLines "../../Data/Day5.txt"

let sample = [|
    "FBFBBFFRLR" // row 44, column 5, seat ID 357.
    "BFFFBBFRRR" // row 70, column 7, seat ID 567.
    "FFFBBBFRRR" // row 14, column 7, seat ID 119.
    "BBFFBBFRLL" // row 102, column 4, seat ID 820.
|]

type Range = {
    Low: int
    High: int
}

let COLUMNRANGE = { Low = 0; High = 7}

let ROWRANGE = { Low = 0; High = 127 }

type RangeType =
| RowRange of Range
| ColumnRange of Range

type RowSpec = RowSpec of string
type ColumnSpec = ColumnSpec of string

type Spec = Spec of Row : RowSpec * Column : ColumnSpec

type BinaryPartition =
| LowerHalf
| UpperHalf

let toSeatSpecification (boardingPass: string) =
    Spec (RowSpec boardingPass.[0..6], ColumnSpec boardingPass.[7..])

let midPoint range = ((range.High - range.Low) + 1) / 2

let partition range binaryPartition =
    match binaryPartition with
    | LowerHalf -> range |> midPoint |> (fun mid -> { range with High = range.High - mid })
    | UpperHalf -> range |> midPoint |> (fun mid -> { range with Low = range.Low + mid })

let restrictRange range specification =
    match specification with
    | 'F'
    | 'L' -> partition range LowerHalf
    | 'B'
    | 'R' -> partition range UpperHalf
    | _ -> failwith (sprintf "[%c] is not a F/L/B/R specification" specification)

let toPoint range =
    if range.High <> range.Low then
        failwith ( sprintf "[%A] range has not be specified to a point" range)
    else range.High

let specifySeatNumber (rowRange) (columnRange) (Spec (row, column)) =
    let RowSpec r, ColumnSpec c = row, column
    (r |> Seq.fold restrictRange rowRange |> toPoint, c |> Seq.fold restrictRange columnRange |> toPoint)

let toSeatId seatNumber =
    ((fst seatNumber) * 8) + (snd seatNumber)

let maxSeatId data =
    data
    |> Array.map (toSeatSpecification >> specifySeatNumber ROWRANGE COLUMNRANGE >> toSeatId)
    |> Array.max
