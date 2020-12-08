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

let toInstructions text = text |> Array.map parseInstruction

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

let bootStatus (instructions: Instruction []) accumulator =
    if Array.contains accumulator.CurrentLine accumulator.ProcessedLines then Cycled accumulator.Accumulation
    elif accumulator.CurrentLine >= instructions.Length then Completed accumulator.Accumulation
    else InProgress

let rec boot accumulator instructions =
    match bootStatus instructions accumulator with
    | Cycled n -> Cycled n
    | Completed n -> Completed n
    | InProgress ->
        match instructions.[accumulator.CurrentLine] with
        | Nop _ -> instructions |> boot (accumulator |> incrementLine 1)
        | Jmp n -> instructions |> boot (accumulator |> incrementLine n)
        | Acc n -> instructions |> boot (accumulator |> incrementLine 1 |> accumulate n)

let defaultBoot = boot { ProcessedLines = [||]; CurrentLine = 0; Accumulation = 0 }

let analyze data =
    data |> toInstructions |> defaultBoot

let isNopOrJmp = function
    | (_, Nop _)
    | (_, Jmp _) -> true
    | (_, Acc _) -> false

let switchNopJmp = function
    | (i, Nop n) -> (i, Jmp n)
    | (i, Jmp n) -> (i, Nop n)
    | (i, Acc n) -> (i, Acc n)

let isCompleted = function
    | InProgress
    | Cycled _ -> false
    | Completed _ -> true

let tweakInstructions (instructions: Instruction []) tweak =
    let updatedInstructions = Array.copy instructions
    Array.set updatedInstructions (fst tweak) (snd tweak)
    updatedInstructions

let allNopOrJmp instructions =
    instructions |> Array.indexed |> Array.filter isNopOrJmp

let findCompletion data =
    let instructions = data |> toInstructions
    instructions
    |> allNopOrJmp
    |> Seq.map (switchNopJmp >> tweakInstructions instructions >> defaultBoot)
    |> Seq.filter isCompleted
    |> Seq.tryHead
