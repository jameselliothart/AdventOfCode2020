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
    let asInt = data |> Seq.map int
    asInt
    |> Seq.skip preambleLength
    |> Seq.mapi (fun i n -> (n, asInt |> Seq.skip i |> Seq.take preambleLength))
    |> Seq.filter (isXmasNumber >> not)
    |> Seq.head

let result = findNonXmasNumber 5 sample
printfn "%A" result
