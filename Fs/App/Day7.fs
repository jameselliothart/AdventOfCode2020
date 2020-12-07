module Day7
open RegexHelpers
open System.Collections.Generic

let sample = [|
    "light red bags contain 1 bright white bag, 2 muted yellow bags."
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
    "bright white bags contain 1 shiny gold bag."
    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
    "faded blue bags contain no other bags."
    "dotted black bags contain no other bags."
|]

type BagColor = BagColor of string // "light red bags"
type Color = Color of string // "light red"
type RuleText = RuleText of string // "1 bright white bag, 2 muted yellow bags." | "no other bags."
type ContentText = ContentText of string // "1 bright white bag" | "2 muted yellow bags."
type BagLimit = { Color: Color; Number: int }
type AllowedBags = BagLimit array
type Rule = { Color: Color; AllowedBags: AllowedBags }
type BagNumber = (int * Color)

let toColorAndRule (specification: string) =
    let colorAndRule = specification.Split(" contain ")
    BagColor colorAndRule.[0], RuleText colorAndRule.[1]

let toColor (BagColor bagColor) = Color (bagColor.Replace(" bags", ""))

let toBagLimit (ContentText contentText) =
    let contentArray = contentText.Split(' ')
    {
        Color = Color (contentArray.[1..2] |> Array.reduce (fun x y -> sprintf "%s %s" x y))
        Number = int(contentArray.[0])
    }

let toAllowedBags (RuleText text) =
    match text.Split(", ") with
    | [|"no other bags."|] -> [||]
    | specs -> specs |> Array.map (ContentText >> toBagLimit)

// let toRule specification: Rule =
//     let bagColor, ruleText = specification |> toColorAndRule
//     { Color = toColor bagColor; AllowedBags = toAllowedBags ruleText }

// let toRules specs = specs |> Array.map toRule

// let getLimit color (rule: Rule) =
//     rule.AllowedBags |> Array.tryFind (fun l -> l.Color = color) |> Option.map (fun x -> x.Number)

// let allowedIn (rules: Rule []) color =
//     rules
//     |> Array.map (fun r -> color, (getLimit color r))
//     |> Array.filter (fun (_, n) -> Option.isSome n)

let getBagNumber (bagNumbers: BagNumber []) color =
    bagNumbers |> Array.find (fun x -> snd x = color) |> fst

let getColor (bagNumbers: BagNumber []) index =
    index |> Array.get bagNumbers |> snd

let ruleRow (bagNumbers: BagNumber []) (allowedBags: AllowedBags) =
    let row = Array.create bagNumbers.Length 0
    allowedBags |> Array.iter (fun x -> Array.set row (getBagNumber bagNumbers x.Color) x.Number)
    row

let ruleMatrix (bagNumbers: BagNumber []) (rules: AllowedBags []) =
    [|
        for rule in rules -> ruleRow bagNumbers rule
    |]

let toRuleMatrix data : (BagNumber [] * int [] []) =
    data
    |> Array.map toColorAndRule
    |> Array.fold
        (fun (allColors, allRules) (b,r) -> ([|toColor b|] |> Array.append allColors, [|toAllowedBags r|] |> Array.append allRules))
        ([||],[||])
    |> fun (colors, rules) -> (colors |> Array.indexed, rules)
    |> fun (bagNumbers, rules) -> bagNumbers, ruleMatrix bagNumbers rules

let containedIn (bagNumbers: BagNumber []) (ruleMatrix: int [] []) (color: Color * int) =
    let index = getBagNumber bagNumbers (fst color)
    ruleMatrix
    |> Array.map (fun r -> r.[index])
    |> Array.indexed
    |> Array.filter (fun (_, n) -> n > 0)
    |> Array.map (fun (i,n) -> getColor bagNumbers i, (snd color) * n)

let rec findAll (bagNumbers: BagNumber []) (ruleMatrix: int [] []) color =
    seq {
        let containedBy = color |> containedIn bagNumbers ruleMatrix
        for limit in containedBy do
            yield limit
            yield! findAll bagNumbers ruleMatrix limit
    }

let bagsCanContain color data =
    let bagNumbers, matrix = data |> toRuleMatrix
    findAll bagNumbers matrix (Color color, 1)
    |> Seq.distinctBy fst
    |> Seq.length

// bagsCanContain sample "shiny gold"

(*
    [|
        [|0; 0; 1; 2; 0; 0; 0; 0; 0|];
        [|0; 0; 3; 4; 0; 0; 0; 0; 0|];
        [|0; 0; 0; 0; 1; 0; 0; 0; 0|];
        [|0; 0; 0; 0; 2; 0; 0; 9; 0|];
        [|0; 0; 0; 0; 0; 1; 2; 0; 0|];
        [|0; 0; 0; 0; 0; 0; 0; 3; 4|];
        [|0; 0; 0; 0; 0; 0; 0; 5; 6|];
        [|0; 0; 0; 0; 0; 0; 0; 0; 0|];
        [|0; 0; 0; 0; 0; 0; 0; 0; 0|]
    |]

(2,1); (3,2)
*)