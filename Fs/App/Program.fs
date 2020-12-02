// Learn more about F# at http://fsharp.org

open System
open System.IO
open Day1
open Day2

[<EntryPoint>]
let main argv =
    let entries = File.ReadAllLines "../../Data/Day2.txt"

    entries
    |> Array.map checkPasswordLine
    |> Array.filter (Option.exists id)
    |> Array.length
    |> printfn "%i"
    0 // return an integer exit code
