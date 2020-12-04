module Day4
open System
open System.Text


let sample = [|
    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
    "byr:1937 iyr:2017 cid:147 hgt:183cm"
    ""
    "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
    "hcl:#cfa07d byr:1929"
    ""
    "hcl:#ae17e1 iyr:2013"
    "eyr:2024"
    "ecl:brn pid:760753108 byr:1931"
    "hgt:179cm"
    ""
    "hcl:#cfa07d eyr:2025 pid:166559648"
    "iyr:2011 ecl:brn hgt:59in"
|]

let mandatoryIdFields = [|
    "byr"
    "iyr"
    "eyr"
    "hgt"
    "hcl"
    "ecl"
    "pid"
|]

let optionalIdFields = [|
    "cid"
|]

type Idenfification =
| Passport
| NorthPoleCredential

let consolidatePassportInfo (accumulatedInfo, (builder: StringBuilder)) line =
    match line with
    | "" ->
        [|builder.ToString()|] |> Array.append accumulatedInfo, StringBuilder()
    | info ->
        accumulatedInfo, builder.Append(sprintf "%s " info)

let toKeyValuePair (fieldValue: string) =
    let split = fieldValue.Split(':')
    (split.[0], split.[1])

let mapFromRawPassportInfo (line: string) =
    line.Trim().Split(' ') |> Array.map toKeyValuePair |> Map.ofArray

let validatePassport (passportInfo: Map<string,string>) =
    match Array.TrueForAll (mandatoryIdFields, fun x -> passportInfo.ContainsKey x) with
    | false -> None
    | true ->
        if Array.TrueForAll (optionalIdFields, fun x -> passportInfo.ContainsKey x)
        then Some Passport
        else Some NorthPoleCredential

let countValidPassports data =
    [|""|]
    |> Array.append data
    |> Array.fold consolidatePassportInfo ([||], StringBuilder())
    |> fst
    |> Array.map (mapFromRawPassportInfo >> validatePassport)
    |> Array.filter Option.isSome
    |> Array.length
