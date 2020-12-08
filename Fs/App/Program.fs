// Learn more about F# at http://fsharp.org

open System
open System.IO
open Day8

[<EntryPoint>]
let main argv =
    File.ReadAllLines "../../Data/Day8.txt"
    |> findCompletion
    |> printfn "%A"
    0 // return an integer exit code
