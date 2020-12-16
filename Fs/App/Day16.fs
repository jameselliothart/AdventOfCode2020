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
    | Regex @"(?<field>\w+): (?<lb1>\d+)-(?<ub1>\d+) or (?<lb2>\d+)-(?<ub2>\d+)" [ field; lb1; ub1; lb2; ub2 ] ->
        (field, fun v -> (v >= (int)lb1 && v <= (int)ub1) || (v >= (int)lb2 && v <= (int)ub2))
    | _ -> failwith (sprintf "failed to parse")

type Validation =
| Invalid of int
| PossiblyValid of int

let invalidValue rules value =
    match rules |> Array.forall (fun r -> not (r value)) with
    | true -> Invalid value
    | false -> PossiblyValid value

let errorNumber = function
    | Invalid x -> x
    | PossiblyValid _ -> 0

let ticketErrorValue values =
    values |> Array.sumBy errorNumber

// sample |> fieldRules
// "row: 6-11 or 33-44" |> toFieldRule
let solve data =
    let rules = data |> fieldRules |> Array.map (toFieldRule >> snd)
    data
    |> nearbyTickets
    |> toTicketValues
    |> Array.map (fun xs -> xs |> Array.map (invalidValue rules))
    |> Array.sumBy ticketErrorValue

data |> solve
