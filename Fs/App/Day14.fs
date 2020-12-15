module Day14
open RegexHelpers
open System

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
| AssignMemory of (int64 * int64)

type MemoryWriter = { Mask: Mask; MemoryMap: Map<int64, int64> }

let toMask (data: string) = Mask (data.Split("mask = ").[1])

let toBit (num: int64) = Convert.ToString(num, 2).PadLeft(36, '0')

let toInt (bits: string) = Convert.ToInt64(bits, 2)

let mask (Mask bitmask) bits =
    bits |> Seq.map2 (fun m b -> if m = 'X' then b else m) bitmask |> String.Concat

let toMemoryAssignment text =
    match text with
    | Regex @"mem\[(?<address>\d+)\] = (?<value>\d+)" [ address; value ] ->
        ((int64)address, (int64)value)
    | _ -> failwith (sprintf "cannot parse %s" text)

let parse (line: string) =
    match line with
    | l when l.StartsWith("mask") -> l |> toMask |> UpdateMask
    | l -> l |> toMemoryAssignment |> AssignMemory

let assignMemory (memory: Map<int64, int64>) bitmask (address, value) =
    (address, value |> toBit |> mask bitmask |> toInt) |> memory.Add

let handle writer command =
    match command with
    | UpdateMask m -> { writer with Mask = m }
    | AssignMemory x -> { writer with MemoryMap = assignMemory writer.MemoryMap writer.Mask x }

let sumMemory (memory: Map<int64, int64>) =
    memory |> Map.toSeq |> Seq.sumBy snd

let solve data =
    data
    |> Array.map (parse)
    |> Array.fold handle { Mask = Mask "X"; MemoryMap = Map.empty }
    |> fun w -> sumMemory w.MemoryMap

let applyMask m b =
    match m with
    | 'X' | '1' -> m
    | _ -> b

type FloatingMemory = FloatingMemory of string
type MemoryAddress = MemoryAddress of string

let mask2 (Mask bitmask) bits =
    bits |> Seq.map2 applyMask bitmask |> String.Concat |> FloatingMemory

let rec cartesian (LL) =
    match LL with
    | [] -> Seq.singleton []
    | L::Ls -> seq {for x in L do for xs in cartesian Ls -> x::xs}

let replaceFirst (find: char) (str: string) replace =
    seq {
        for (i, char) in Seq.indexed str do
            if i = str.IndexOf(find) then yield replace
            else yield char
    }
    |> String.Concat

let toAddresses (FloatingMemory mem) =
    let numFloats = mem |> Seq.filter ((=) 'X') |> Seq.length
    ['0';'1']
    |> List.replicate numFloats
    |> cartesian
    |> Seq.map (List.fold (replaceFirst 'X') mem >> MemoryAddress)

let assignMemory2 (memory: Map<int64, int64>) bitmask (address, (value: int64)) =
    address
    |> mask2 bitmask
    |> toAddresses
    |> Seq.map (fun (MemoryAddress m) -> (toInt m, value))
    |> Seq.fold (fun (s: Map<int64, int64>) t -> t |> s.Add ) memory

let handle2 writer command =
    match command with
    | UpdateMask m -> { writer with Mask = m }
    | AssignMemory (k,v) -> { writer with MemoryMap = assignMemory2 writer.MemoryMap writer.Mask (toBit k,v) }

let sample2 = [|
    "mask = 000000000000000000000000000000X1001X"
    "mem[42] = 100"
    "mask = 00000000000000000000000000000000X0XX"
    "mem[26] = 1"
|]

let solve2 data =
    data
    |> Array.map parse
    |> Array.fold handle2 { Mask = Mask "X"; MemoryMap = Map.empty }
    |> fun w -> sumMemory w.MemoryMap
