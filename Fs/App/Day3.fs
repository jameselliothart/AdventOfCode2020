module Day3

let sample =
    [| "..##.......";"#...#...#..";".#....#..#.";"..#.#...#.#";".#...##..#.";"..#.##.....";".#.#.#....#";".#........#";"#.##...#...";"#...##....#";".#..#...#.#"; |]

type Position = {
    Row: int
    Column: int
}

type Forest = string array

type Slope = {
    Rise: int
    Run: int
}

type TreeCount = {
    Count: int64
    Position: Position option
}

let countTree (forest: Forest) position =
    match forest.[position.Row].[position.Column] with
    | '#' -> 1
    | _ -> 0
    |> int64

let move (forest: Forest) slope position =
    let row = position.Row + slope.Rise
    if row > forest.Length-1 then None else
        let column = (position.Column + slope.Run) % (forest.[row].Length)
        Some { Row = row; Column = column }

let countTreeAndMove (forest: Forest) slope treeCount =
    match treeCount.Position with
    | None -> treeCount
    | Some position ->
        let count = treeCount.Count + (countTree forest position)
        let newPosition = move forest slope position
        { Count = count; Position = newPosition }

let rec traverse (forest: Forest) treeCount slope =
    let newTreeCount = countTreeAndMove forest slope treeCount
    match newTreeCount.Position with
    | None -> newTreeCount.Count
    | Some _ -> traverse forest newTreeCount slope

let slope = { Rise = 1; Run = 3 }
let slopes = [|
    { Rise = 1; Run = 1 }
    { Rise = 1; Run = 3 }
    { Rise = 1; Run = 5 }
    { Rise = 1; Run = 7 }
    { Rise = 2; Run = 1 }
|]
let initialCount = { Count = int64(0); Position = Some { Row = 0; Column = 0 } }

// sample
// |> Array.fold (countTreeAndMove (Forest sample) { Rise = 1; Run = 3 }) { Count = 0; Position = { Row = 0; Column = 0 } }


// 6
