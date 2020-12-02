module Day1

let sample = [|1721; 979; 366; 299; 675; 1456|]

let rec recProductOfEntries condition product entries =
    match entries with
    | [] -> Seq.empty
    | head :: tail ->
        let candidate =
            tail
            |> Seq.map (fun x -> if (condition head x) then Some(product head x) else None)
            |> Seq.filter Option.isSome
        if Seq.isEmpty candidate then recProductOfEntries condition product tail
        else candidate

let recProductOfTwo2020Entries = recProductOfEntries (fun x y -> x + y = 2020) ( * )

let rec recProductOfThree2020Entries entries =
    match entries with
    | [] -> Seq.empty
    | head :: tail ->
        let candidate = recProductOfEntries (fun x y -> head + x + y = 2020) (fun x y -> head * x * y) tail
        if Seq.isEmpty candidate then recProductOfThree2020Entries tail
        else candidate

let productOfTwo2020Entries (entries: int []) =
    seq {
        for i,a in Array.indexed entries do
            for b in entries.[i..] do
                if a+b = 2020 then a*b
    }
    |> Seq.exactlyOne

let productOfThree2020Entries (entries: int array) =
    seq {
        for i,a in Array.indexed entries do
            for b in entries.[i..] do
                for c in entries.[i+1..] do
                    if a+b+c = 2020 then a*b*c
    }
    |> Seq.take 1
    |> Seq.exactlyOne
