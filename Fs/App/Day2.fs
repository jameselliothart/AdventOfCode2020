module Day2

open System.Text.RegularExpressions

let sample = [|"1-3 a: abcde"; "1-3 b: cdefg"; "2-9 c: ccccccccc" |]

type PasswordPolicy = {
    Min: int
    Max: int
    Letter: char
}

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parse line =
    match line with
    | Regex @"(?<min>\d+)-(?<max>\d+) (?<letter>\w): (?<password>\w+)" [ min; max; letter; password ] ->
        Some ({ Min = int(min); Max = int(max); Letter = char(letter) }, password)
    | _ ->
        printfn "Cannot parse [%s]" line
        None

let goodPassword policy password =
    password
    |> Seq.filter ((=) policy.Letter)
    |> Seq.length
    |> fun n -> policy.Min <= n && n <= policy.Max

let checkPasswordLine = parse >> Option.map (fun y -> goodPassword (fst y) (snd y))
