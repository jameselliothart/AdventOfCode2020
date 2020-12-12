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

type Turn = L of int | R of int

type Direction = N | E | S | W

type Action =
| Move of Direction * int
| Turn of Turn
| F of int

type Position = { Facing: Direction; NorthSouth: int; EastWest: int }

let toDirection = function
    | d when d = 0 -> E
    | d when d = 90 -> N
    | d when d = 180 -> W
    | d when d = 270 -> S
    | d -> failwith (sprintf "cannot parse %i" d)

let toDegree = function
    | E -> 0
    | N -> 90
    | W -> 180
    | S -> 270

let turn leftright starting =
    match leftright with
    | L d -> (starting + d) % 360
    | R d -> (starting - d + 360) % 360

let turnDirection leftright starting =
    starting |> toDegree |> turn leftright |> toDirection

let toInstruction (text: string) =
    let action, value = text.[0], int(text.[1..])
    match action with
    | a when a = 'N' -> Move (N, value)
    | a when a = 'S' -> Move (S, value)
    | a when a = 'E' -> Move (E, value)
    | a when a = 'W' -> Move (W, value)
    | a when a = 'L' -> Turn (L value)
    | a when a = 'R' -> Turn (R value)
    | a when a = 'F' -> F value
    | _ -> failwith (sprintf "cannot parse %c" action)

let rec followInstruction position instruction =
    match instruction with
    | Move (N, v) -> { position with NorthSouth = position.NorthSouth + v}
    | Move (S, v) -> { position with NorthSouth = position.NorthSouth - v}
    | Move (E, v) -> { position with EastWest = position.EastWest + v}
    | Move (W, v) -> { position with EastWest = position.EastWest - v}
    | Turn (L v) -> { position with Facing = position.Facing |> turnDirection (L v) }
    | Turn (R v) -> { position with Facing = position.Facing |> turnDirection (R v) }
    | F v -> followInstruction position (Move (position.Facing, v))

let manhattanDifference position =
    (System.Math.Abs position.NorthSouth) + (System.Math.Abs position.EastWest)

let solve data =
    data
    |> Array.map toInstruction
    |> Array.fold followInstruction { Facing = E; NorthSouth = 0; EastWest = 0; }
    |> manhattanDifference
