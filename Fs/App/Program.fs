// Learn more about F# at http://fsharp.org

open System
open System.IO
open Day4

[<EntryPoint>]
let main argv =
    let entries = File.ReadAllLines "../../Data/Day4.txt"

    entries
    |> countValidPassports
    |> printfn "%i"
    0 // return an integer exit code
