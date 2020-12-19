module Day18

type Symbol =
| Number of int64
| Add
| Multiply

type Calculation =
| Operand of int64
| Operation of (int64 -> int64)

type ExpressionElement =
| Symbol of Symbol
| Begin
| End

type Calculator = Calculation -> Symbol -> Calculation

let calculate soFar next =
    match (soFar, next) with
    | (Operation _, Add) | (Operation _, Multiply) -> failwith "adjacent operators"
    | (Operation f, Number x) -> Operand (f x)
    | (Operand _, Number x) -> Operand x
    | (Operand x, Add) -> Operation ((+) x)
    | (Operand x, Multiply) -> Operation ((*) x)

let toNumber = function
    | Operand x -> Number x
    | Operation _ -> failwith "cannot extract number from operation"

let rec accumulate (calculator: Calculator) soFar expression =
    match expression with
    | next :: rest ->
        match next with
        | End -> ([soFar |> toNumber |> Symbol] @ rest)
        | Begin -> accumulate calculator soFar (accumulate calculator (Operation id) rest)
        | Symbol s ->
            let calculation = calculator soFar s
            accumulate calculator calculation rest
    | [] -> [soFar |> toNumber |> Symbol]

let toSymbol = function
    | x when x = '(' -> Begin
    | x when x = ')' -> End
    | x when x = '+' -> Add |> Symbol
    | x when x = '*' -> Multiply |> Symbol
    | x -> x |> string |> int64 |> Number |> Symbol

let toSymbols symbolMaker text =
    text
    |> Seq.filter ((<>) ' ')
    |> Seq.map symbolMaker
    |> List.ofSeq

let solve text =
    text
    |> toSymbols toSymbol
    |> accumulate calculate (Operation id)

let extractOperand = function
    | Symbol (Number x) -> x
    | _ -> failwith "cannot extract from Operation"

let data () = System.IO.File.ReadAllLines "./Data/Day18.txt"

let sample = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"  // 13632

// sample |> solve

data ()
|> Array.sumBy (solve >> List.head >> extractOperand)
