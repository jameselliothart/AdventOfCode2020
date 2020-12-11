// Learn more about F# at http://fsharp.org

open System
open System.IO
open Day10

[<EntryPoint>]
let main argv =
    File.ReadAllLines "../../Data/Day10.txt"
    |> solve2
    |> printfn "%A"
    0 // return an integer exit code
