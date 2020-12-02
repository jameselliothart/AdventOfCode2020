// Learn more about F# at http://fsharp.org

open System
open Day1
open System.IO


[<EntryPoint>]
let main argv =
    let entries = File.ReadAllLines "../../Data/Day1.txt"

    entries
    |> Array.map int
    |> productOfThree2020Entries
    |> printfn "%i"
    0 // return an integer exit code
