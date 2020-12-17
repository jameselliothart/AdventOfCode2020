module Day16
open RegexHelpers

let data = System.IO.File.ReadAllLines "./Data/Day16.txt"

let sample = [|
    "class: 1-3 or 5-7"
    "row: 6-11 or 33-44"
    "seat: 13-40 or 45-50"
    ""
    "your ticket:"
    "7,1,14"
    ""
    "nearby tickets:"
    "7,3,47"
    "40,4,50"
    "55,2,20"
    "38,6,12"
|]

let nearbyTickets data =
    let nearbyTicketsStart = data |> Array.findIndex ((=) "nearby tickets:") |> (+) 1
    data |> Array.skip nearbyTicketsStart

let toTicketValues (tickets: string []) =
    tickets
    |> Array.map (fun t -> t.Split(',') |> Array.map int)

let fieldRules data =
    data |> Array.takeWhile ((<>) "")

let toFieldRule = function
    | Regex @"(?<field>.*): (?<lb1>\d+)-(?<ub1>\d+) or (?<lb2>\d+)-(?<ub2>\d+)" [ field; lb1; ub1; lb2; ub2 ] ->
        (field, fun v -> (v >= (int)lb1 && v <= (int)ub1) || (v >= (int)lb2 && v <= (int)ub2))
    | _ -> failwith (sprintf "failed to parse")

type Validation =
| Invalid of int
| Valid of int

let invalidValue rules value =
    match rules |> Array.forall (fun r -> not (r value)) with
    | true -> Invalid value
    | false -> Valid value

let errorNumber = function
    | Invalid x -> x
    | Valid _ -> 0

let ticketErrorValue values =
    values |> Array.sumBy errorNumber

let solve data =
    let rules = data |> fieldRules |> Array.map (toFieldRule >> snd)
    data
    |> nearbyTickets
    |> toTicketValues
    |> Array.map (fun xs -> xs |> Array.map (invalidValue rules))
    |> Array.sumBy ticketErrorValue

let sample2 = [|
    "class: 0-1 or 4-19"
    "row: 0-5 or 8-19"
    "seat: 0-13 or 16-19"
    ""
    "your ticket:"
    "11,12,13"
    ""
    "nearby tickets:"
    "3,9,18"
    "15,1,5"
    "5,14,9"
|]

let toValidTickets rules ticketValues =
    ticketValues
    |> Array.filter (fun xs ->
        xs
        |> Array.map ((fun x -> rules |> Array.forall (fun r -> not (r x))) >> not)
        |> Array.forall id
    )

let determineField rules (tickets: int [] []) fieldIndex =
    let isolatedField = tickets |> Array.map (fun xs -> xs.[fieldIndex])
    rules
    |> Array.where (fun (_, r) -> Array.forall r isolatedField)
    |> fun rs -> (fieldIndex, rs |> Array.map fst)

let rec decodeFields (mapping) (rules) (tickets) =
    let numMappings = Array.length mapping
    if numMappings = (tickets |> Array.head |> Array.length) then mapping
    else
        determineField rules tickets numMappings
        |> fun (_, fs) -> decodeFields ([|fs|] |> Array.append mapping) rules tickets

let rec flattenPossibilities mappings =
    let singles = mappings |> Array.where (Array.length >> (=) 1) |> Array.map Array.head
    let flattened = mappings |> Array.map (fun fs -> if fs.Length > 1 then (fs |> Array.except singles) else fs)
    if singles.Length = flattened.Length then flattened
    else flattenPossibilities flattened

let solve2 data =
    let rules = data |> fieldRules |> Array.map toFieldRule
    data
    |> nearbyTickets
    |> toTicketValues
    |> toValidTickets (rules |> Array.map snd)
    |> decodeFields [||] rules
    |> flattenPossibilities
    |> Array.map Array.head
    |> Array.indexed
    |> Array.filter (fun (_, f) -> f.Contains "departure")

let myTicket =
    [|"223,139,211,131,113,197,151,193,127,53,89,167,227,79,163,199,191,83,137,149"|]
    |> toTicketValues
    |> Array.head
    |> Array.map int64

data |> solve2

[myTicket.[4]; myTicket.[10]; myTicket.[14]; myTicket.[15]; myTicket.[17]; myTicket.[18]] |> List.reduce (*)
