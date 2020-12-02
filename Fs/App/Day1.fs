module Day1

let sample = [|1721; 979; 366; 299; 675; 1456|]

let productOfTwo2020Entries (entries: int array) =
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
