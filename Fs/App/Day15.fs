module Day15

let toFormat1 data =
    data |> List.indexed |> List.map (fun (i,n) -> (i+1,n)) |> List.rev

let data = [13;16;0;12;15;1]
let sample = [0;3;6]

let data1 = data |> toFormat1
let sample1 = sample |> toFormat1

let speak previouslySpoken =
    match previouslySpoken with
    | [] -> failwith "cannot pass empty list"
    | head :: tail ->
        let round = (fst head) + 1
        let next =
            match tail |> List.tryFind (snd >> ((=) (snd head))) with
            | Some (i,_) -> (round, (fst head) - i)
            | None -> (round, 0)
        [next] @ previouslySpoken

let solve ending (data: (int * int) list) =
    [1..(ending - data.Length)]
    |> List.fold (fun x _ -> speak x) data
    |> List.head

type Round = { CurrentRound: int; LastSpoken: int; PreviousRounds: Map<int, int []> }

let toFormat2 (data: int list) =
    {
        CurrentRound = data.Length + 1
        LastSpoken = data |> List.last
        PreviousRounds = data |> List.indexed |> List.map (fun (i,n) -> (n,[|i+1|])) |> Map.ofList
    }

let toNextRound round = { round with CurrentRound = round.CurrentRound + 1 }

let toSpeak round number =
    let appendOn = round.PreviousRounds |> Map.tryFind number |> Option.defaultValue [||]
    { round with
        LastSpoken = number
        PreviousRounds = round.PreviousRounds |> Map.add number (appendOn |> Array.append [|round.CurrentRound|])
    }

let speak2 round =
    match round.PreviousRounds |> Map.tryFind round.LastSpoken with
    | Some i ->
        if i.Length = 1 then 0
        else (i.[0] - i.[1])
    | None -> 0
    |> toSpeak round
    |> toNextRound

let solve2 ending data =
    let start = data |> toFormat2
    Seq.init ending ((+) 1)
    |> Seq.skip data.Length
    |> Seq.fold (fun x _ -> speak2 x) start
    |> fun x -> x.LastSpoken

// 30000000

data
|> solve2 2020
|> printfn "%A"
