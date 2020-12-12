module Day12

let sample = [|
    "F10"
    "N3"
    "F7"
    "R90"
    "F11"
|]

(*
E = 0
N = 90
W = 180
S = 270
*)

type Coordinate = int * int

type Instruction =
| Move of Coordinate
| Turn of int
| F of int

type Position = { Direction: Coordinate; Coordinate: Coordinate }

let toInstruction (text: string) =
    let action, value = text.[0], int(text.[1..])
    match action with
    | a when a = 'N' -> Move (0, value)
    | a when a = 'S' -> Move (0, -value)
    | a when a = 'E' -> Move (value, 0)
    | a when a = 'W' -> Move (-value, 0)
    | a when a = 'L' -> Turn (value/90)
    | a when a = 'R' -> Turn (-value/90)
    | a when a = 'F' -> F value
    | _ -> failwith (sprintf "cannot parse %c" action)

let times value coordinate =
    ( value * (fst coordinate), value * (snd coordinate) )

let add coord1 coord2 =
    ( (fst coord1) + (fst coord2), (snd coord1) + (snd coord2) )

let turn starting times =
    let directions = [|(1,0); (0,1); (-1,0); (0,-1)|]
    let startingIndex = directions |> Array.findIndex ((=) starting)
    let index = (startingIndex + times + directions.Length) % directions.Length
    directions.[index]

let rec followInstruction position instruction =
    match instruction with
    | Move c -> { position with Coordinate = add c position.Coordinate }
    | Turn d -> { position with Direction = turn position.Direction d }
    | F v -> followInstruction position (Move (times v position.Direction))

let manhattanDifference (coordinate: Coordinate) =
    (System.Math.Abs (fst coordinate)) + (System.Math.Abs (snd coordinate))

let solve data =
    data
    |> Array.map toInstruction
    |> Array.fold followInstruction { Direction = (1,0); Coordinate = (0,0) }
    |> fun p -> manhattanDifference p.Coordinate

let rec repeat n f x = if n=0 then x else repeat (n-1) f (f x)
let swap (x,y) = (y,x)
let multCoordinate coord1 coord2 = ( (fst coord1) * (fst coord2), (snd coord1) * (snd coord2) )
let ccw = swap >> multCoordinate (-1,1)
let cw = swap >> multCoordinate (1,-1)

let rotate coordinate times =
    match times with
    | r when r < 0 -> repeat -r cw coordinate
    | l -> repeat l ccw coordinate

type PositionWithWaypoint = { ShipPosition: Coordinate; WaypointPosition: Coordinate }

let followWaypointInstruction positions instruction =
    match instruction with
    | Move c -> { positions with WaypointPosition = add c positions.WaypointPosition }
    | Turn d -> { positions with WaypointPosition = rotate positions.WaypointPosition d }
    | F v -> { positions with ShipPosition = positions.WaypointPosition |> times v |> add positions.ShipPosition }

let solve2 data =
    data
    |> Array.map toInstruction
    |> Array.fold followWaypointInstruction { ShipPosition = (0,0); WaypointPosition = (10,1) }
    |> fun p -> manhattanDifference p.ShipPosition

System.IO.File.ReadAllLines "./Data/Day12.txt"
|> solve2
|> printfn "%A"

// (10,4)
// (4,-10)
// (-10,-4)
// (-4,10)
