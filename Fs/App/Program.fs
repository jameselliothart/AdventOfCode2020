// Learn more about F# at http://fsharp.org

open System
open System.IO
open Day12

[<EntryPoint>]
let main argv =
    File.ReadAllLines "../../Data/Day12.txt"
    |> solve
    |> printfn "%A"
    0 // return an integer exit code
