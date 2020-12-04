module Day4
open System
open System.Text
open System.Text.RegularExpressions
open RegexHelpers


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

type Rule = string -> bool

let validateHeight value =
    match value with
    | Regex @"(?<number>\d+)(?<unit>\w+)" [ number; unit ] ->
        match unit with
        | "cm" -> (int)number >= 150 && (int)number <= 193
        | "in" -> (int)number >= 59 && (int)number <= 76
        | _ -> false
    | _ -> false

let (mandatoryIdFields: (string * Rule) array) =
    [|
        "byr", fun x -> (x.Length = 4) && ((int)x >= 1920 && (int)x <= 2002) //(Birth Year) - four digits; at least 1920 and at most 2002.
        "iyr", fun x -> (x.Length = 4) && ((int)x >= 2010 && (int)x <= 2020) //(Issue Year) - four digits; at least 2010 and at most 2020.
        "eyr", fun x -> (x.Length = 4) && ((int)x >= 2020 && (int)x <= 2030) //(Expiration Year) - four digits; at least 2020 and at most 2030.
        "hgt", validateHeight //(Height) - a number followed by either cm or in:
            //If cm, the number must be at least 150 and at most 193.
            //If in, the number must be at least 59 and at most 76.
        "hcl", fun x -> Regex.Match(x, "^#[0-9|a-f]{6}$").Success //(Hair Color) - a # followed by exactly six characters 0-9 or a-f.
        "ecl", fun x -> [|"amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"|] |> Array.contains x //(Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
        "pid", fun x -> Regex.Match(x, "^\d{9}$").Success //(Passport ID) - a nine-digit number, including leading zeroes.
    |]

let optionalIdFields = [|
    "cid"
|]

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

let validatePassportFields (passportInfo: Map<string,string>) =
    match Array.TrueForAll (mandatoryIdFields |> Array.map fst, fun x -> passportInfo.ContainsKey x) with
    | false -> None
    | true -> Some (passportInfo)

let validatePassportValues (rules: (string * Rule) array) (passportInfo: Map<string,string>) =
    Array.TrueForAll (rules, fun r -> (snd r) (passportInfo.[(fst r)]))

let countValidPassports data =
    [|""|]
    |> Array.append data
    |> Array.fold consolidatePassportInfo ([||], StringBuilder())
    |> fst
    |> Array.map (mapFromRawPassportInfo >> validatePassportFields)
    |> Array.filter (Option.exists (validatePassportValues mandatoryIdFields))
    |> Array.length

let inValidTest = [|
    "eyr:1972 cid:100"
    "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
    ""
    "iyr:2019"
    "hcl:#602927 eyr:1967 hgt:170cm"
    "ecl:grn pid:012533040 byr:1946"
    ""
    "hcl:dab227 iyr:2012"
    "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
    ""
    "hgt:59cm ecl:zzz"
    "eyr:2038 hcl:74454a iyr:2023"
    "pid:3556412378 byr:2007"
|]

let validTest = [|
    "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
    "hcl:#623a2f"
    ""
    "eyr:2029 ecl:blu cid:129 byr:1989"
    "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
    ""
    "hcl:#888785"
    "hgt:164cm byr:2001 iyr:2015 cid:88"
    "pid:545766238 ecl:hzl"
    "eyr:2022"
    ""
    "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
|]
