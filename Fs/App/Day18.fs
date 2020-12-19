module Day18

type Symbol =
| Number of int
| Add
| Multiply

type Calculation =
| Operand of int
| Operation of (int -> int)

let calculate soFar next =
    match (soFar, next) with
    | (Operation _, Add) | (Operation _, Multiply) -> failwith "adjacent operators"
    | (Operation f, Number x) -> Operand (f x)
    | (Operand _, Number x) -> Operand x
    | (Operand x, Add) -> Operation ((+) x)
    | (Operand x, Multiply) -> Operation ((*) x)

let rec accumulate calculator soFar expression =
    match expression with
    | [] -> soFar
    | next :: rest ->
        let calculation = calculator soFar next
        accumulate calculator calculation rest

let toSymbol = function
    | x when x = '+' -> Add
    | x when x = '*' -> Multiply
    | x -> x |> string |> int |> Number

let toSymbols symbolMaker text =
    text
    |> Seq.filter ((<>) ' ')
    |> Seq.map symbolMaker
    |> List.ofSeq

"1 + 2 * 3 + 4 * 5 + 6"
|> toSymbols toSymbol
|> accumulate calculate (Operation id)


// type Expression =
// | CharacterSequence of Character seq
// | Subexpression of Expression

// let toCharacter = function
//     | x when x = '+' -> Add
//     | x when x = '*' -> Multiply

//     | x -> x |> int |> Digit

// type Expression = { SoFar: int; Operator: int -> int -> int; Next: Expression option }

// let calculate stored operator next =
//     operator stored next

// let calculator stored operator next =
//     match next with
//     | Digit n -> calculate (operator stored n) operator
//     | Add -> calculate stored (+)
//     | Multiply -> calculate stored (*)



// let number expression =
//     let mutable level = 0
//     [
//         for x in expression do
//             if x = '(' then level <- level + 1
//             elif x = ')' then level <- level - 1
//             else yield (level, x)
//     ]

// let reduce acc numbered =
//     let max = numbered |> List.maxBy fst |> fst
//     [
//         for (n,d) in numbered do
//             if n < max then yield (n,d)
//             else
//     ]

// "1 + 2 * 3 + 4 * 5 + 6".Replace(" ", "")
// |> List.ofSeq
// |> number


// "1 + (2 * 3) + (4 * (5 + 6))".Replace(" ", "")
// |> List.ofSeq
// |> number
// |> List.maxBy fst
// |> fst