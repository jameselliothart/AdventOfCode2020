// Learn more about F# at http://fsharp.org

open System
open System.IO
open Day11

[<EntryPoint>]
let main argv =
    File.ReadAllLines "../../Data/Day11.txt"
    |> solve
    |> printfn "%A"
    0 // return an integer exit code
