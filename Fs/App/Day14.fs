module Day14
open System
open RegexHelpers

let data = IO.File.ReadAllLines "./Data/Day14.txt"

let sample = [|
    "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
    "mem[8] = 11"
    "mem[7] = 101"
    "mem[8] = 0"
|]

type Mask = Mask of string

type Command =
| UpdateMask of Mask
| AssignMemory of (int * int64)

type MemoryWriter = { Mask: Mask; MemoryMap: Map<int, int64> }

let toMask (data: string) = Mask (data.Split("mask = ").[1])

let toBit (num: int64) = Convert.ToString(num, 2).PadLeft(36, '0')

let toInt (bits: string) = Convert.ToInt64(bits, 2)

let mask (Mask bitmask) bits =
    bits |> Seq.map2 (fun m b -> if m = 'X' then b else m) bitmask |> String.Concat

let toMemoryAssignment text =
    match text with
    | Regex @"mem\[(?<address>\d+)\] = (?<value>\d+)" [ address; value ] ->
        ((int)address, (int64)value)
    | _ -> failwith (sprintf "cannot parse %s" text)

let parse (line: string) =
    match line with
    | l when l.StartsWith("mask") -> l |> toMask |> UpdateMask
    | l -> l |> toMemoryAssignment |> AssignMemory

let assignMemory (memory: Map<int, int64>) bitmask (address, value) =
    (address, value |> toBit |> mask bitmask |> toInt) |> memory.Add

let handle writer command =
    match command with
    | UpdateMask m -> { writer with Mask = m }
    | AssignMemory x -> { writer with MemoryMap = assignMemory writer.MemoryMap writer.Mask x }

let sumMemory (memory: Map<int, int64>) =
    memory |> Map.toSeq |> Seq.sumBy snd

data
|> Array.map (parse)
|> Array.fold handle { Mask = Mask "X"; MemoryMap = Map.empty }
|> fun w -> sumMemory w.MemoryMap
