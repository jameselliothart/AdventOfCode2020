module Day9

let sample = [|
    "35"
    "20"
    "15"
    "25"
    "47"
    "40"
    "62"
    "55"
    "65"
    "95"
    "102"
    "117"
    "150"
    "182"
    "127"
    "219"
    "299"
    "277"
    "309"
    "576"
|]

let isXmasNumber ((number), (preamble)) =
    seq {
        for i,a in Seq.indexed preamble do
            for b in preamble |> Seq.skip i do
                a+b = number
    }
    |> Seq.exists id

let findNonXmasNumber preambleLength data =
    data
    |> Seq.skip preambleLength
    |> Seq.mapi (fun i n -> (n, data |> Seq.skip i |> Seq.take preambleLength))
    |> Seq.filter (isXmasNumber >> not)
    |> Seq.head
    |> fst

let tryfindContiguousSum (xmasNumber: int) data =
    data
    |> Seq.mapi (fun i _ -> data |> Seq.take (i+1))
    |> Seq.takeWhile (fun x -> x |> Seq.sum <= xmasNumber)
    |> Seq.tryPick (fun ns -> if ns |> Seq.sum = xmasNumber then Some ns else None)

let findContiguousSum (data: int seq) xmasNumber =
    data
    |> Seq.mapi (fun i _ -> data |> Seq.skip i)
    |> Seq.map (tryfindContiguousSum xmasNumber)
    |> Seq.tryPick id

let getEncryptionWeakness numbers =
    numbers |> Option.map (fun ns -> (Seq.min ns) + (Seq.max ns))

let findWeakness preambleLength data =
    data
    |> findNonXmasNumber preambleLength
    |> findContiguousSum data
    |> getEncryptionWeakness

let solve1 (data: string []) =
    data |> Seq.map int |> findNonXmasNumber 25

let solve2 (data: string []) =
    data |> Seq.map int |> findWeakness 25

let result = solve2 sample
printfn "%A" result

// tryfindContiguousSum 127 (seq [15; 25; 47; 40; ]);;