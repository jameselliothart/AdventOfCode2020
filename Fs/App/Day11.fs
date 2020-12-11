module Day11

let sample = [|
    "L.LL.LL.LL"
    "LLLLLLL.LL"
    "L.L.L..L.."
    "LLLL.LL.LL"
    "L.LL.LL.LL"
    "L.LLLLL.LL"
    "..L.L....."
    "LLLLLLLLLL"
    "L.LLLLLL.L"
    "L.LLLLL.LL"
|]

type Seat =
| Empty of int * int
| Occupied of int * int
| Floor

type SeatChange =
| BecomeOccupied of int * int
| BecomeEmpty of int * int
| NoChange

let toSeatLayout (data: string []) =
    [|
        for (rowNum, row) in Array.indexed data do
            [|
                for (colNum, char) in Seq.indexed row do
                    match char with
                    | '.' -> Floor
                    | 'L' -> Empty (rowNum,colNum)
                    | '#' -> Occupied (rowNum,colNum)
                    | _ -> failwith (sprintf "cannot parse %c" char)
            |]
    |]

let occupied = function
    | Floor | Empty _ -> false
    | Occupied _ -> true

let totalOccupied seatLayout =
    seatLayout
    |> Array.sumBy (fun r -> r |> Array.filter occupied |> Array.length)

let adjacentOccupied seatLayout rowNum colNum =
    seq {
        for r in [rowNum-1;rowNum;rowNum+1] do
            for c in [colNum-1;colNum;colNum+1] do
                if (r,c) <> (rowNum,colNum) then
                    seatLayout |> Array.tryItem r |> Option.bind (fun row -> row |> Array.tryItem c)
    }
    |> Seq.filter (fun s -> Option.exists occupied s)
    |> Seq.length

let checkSeat occupancyCheck (seatLayout: Seat [] []) seat =
    match seat with
    | Floor -> NoChange
    | Empty (r, c) ->
        if occupancyCheck seatLayout r c = 0 then BecomeOccupied (r,c)
        else NoChange
    | Occupied (r, c) ->
        if occupancyCheck seatLayout r c >= 4 then BecomeEmpty (r,c)
        else NoChange

let checkSeats checkSeat seatLayout =
    seq {
        for row in seatLayout do
            for seat in row do
                yield checkSeat seatLayout seat
    }

let updateSeat (seatLayout: Seat [] []) updatedSeat =
    match updatedSeat with
    | Floor -> seatLayout
    | Empty (r,c) ->
        [|
            for (rowNum, row) in Array.indexed seatLayout do
                [|
                    for (colNum, seat) in Array.indexed row do
                        if (r,c) = (rowNum,colNum) then updatedSeat else seat
                |]
        |]
    | Occupied (r,c) ->
        [|
            for (rowNum, row) in Array.indexed seatLayout do
                [|
                    for (colNum, seat) in Array.indexed row do
                        if (r,c) = (rowNum,colNum) then updatedSeat else seat
                |]
        |]

let occupy seatLayout seatChange =
    match seatChange with
    | NoChange -> seatLayout
    | BecomeEmpty (r,c) -> updateSeat seatLayout (Empty(r,c))
    | BecomeOccupied (r,c) -> updateSeat seatLayout (Occupied(r,c))

let round checkSeats occupy seatLayout =
    seatLayout
    |> checkSeats
    |> Seq.fold occupy seatLayout

let rec getStableLayout round seatLayout =
    match round seatLayout with
    | newLayout when newLayout = seatLayout -> newLayout
    | newLayout -> getStableLayout round newLayout

let solve data =
    data
    |> toSeatLayout
    |> getStableLayout (round (checkSeats (checkSeat adjacentOccupied)) occupy)
    |> totalOccupied
