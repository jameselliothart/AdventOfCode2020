module Day15

let data = [13;16;0;12;15;1] |> List.indexed |> List.map (fun (i,n) -> (i+1,n)) |> List.rev

let sample = [0;3;6] |> List.indexed |> List.map (fun (i,n) -> (i+1,n)) |> List.rev

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

let solve (data: (int * int) list) =
    [1..(2020 - data.Length)]
    |> List.fold (fun x _ -> speak x) data
    |> List.head

data
|> solve
|> printfn "%A"
