module Day8

let sample = [|
    "nop +0"
    "acc +1"
    "jmp +4"
    "acc +3"
    "jmp -3"
    "acc -99"
    "acc +1"
    "jmp -4"
    "acc +6"
|]

type Instruction =
| Nop of int
| Jmp of int
| Acc of int

let parseInstruction (text: string) =
    match text.Split(' ') with
    | [| "acc"; x |] -> Acc ((int)x)
    | [| "jmp"; x |] -> Jmp ((int)x)
    | [| "nop"; x |] -> Nop ((int)x)
    | _ -> failwith (sprintf "cannot parse %s" text)

type Accumulator = { ProcessedLines: int array; CurrentLine: int; Accumulation: int }

let incrementLine amount accumulator =
    { accumulator with
        ProcessedLines = [|accumulator.CurrentLine|] |> Array.append accumulator.ProcessedLines
        CurrentLine = accumulator.CurrentLine + amount
    }

let accumulate amount accumulator = { accumulator with Accumulation = accumulator.Accumulation + amount }

type BootStatus =
| Cycled of int
| Completed of int
| InProgress

let bootStatus (instructions: string []) accumulator =
    if Array.contains accumulator.CurrentLine accumulator.ProcessedLines then Cycled accumulator.Accumulation
    elif accumulator.CurrentLine >= instructions.Length then Completed accumulator.Accumulation
    else InProgress

let rec boot (instructions: string []) accumulator =
    match bootStatus instructions accumulator with
    | Cycled n -> Cycled n
    | Completed n -> Completed n
    | InProgress ->
        match parseInstruction instructions.[accumulator.CurrentLine] with
        | Nop _ -> accumulator |> incrementLine 1 |> boot instructions
        | Jmp n -> accumulator |> incrementLine n |> boot instructions
        | Acc n -> accumulator |> incrementLine 1 |> accumulate n |> boot instructions

let analyze data =
    { ProcessedLines = [||]; CurrentLine = 0; Accumulation = 0 } |> boot data

sample |> analyze |> printfn "%A"
