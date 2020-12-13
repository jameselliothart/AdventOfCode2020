// Learn more about F# at http://fsharp.org

open System
open System.IO
open Day13

[<EntryPoint>]
let main argv =
    File.ReadAllLines "../../Data/Day13.txt"
    |> solve2
    |> printfn "%A"
    0 // return an integer exit code
