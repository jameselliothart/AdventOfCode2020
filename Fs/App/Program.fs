// Learn more about F# at http://fsharp.org

open System
open System.IO
open Day6

[<EntryPoint>]
let main argv =
    File.ReadAllLines "../../Data/Day6.txt"
    |> totalAffirmative
    |> printfn "%A"
    0 // return an integer exit code
