// Learn more about F# at http://fsharp.org

open System
open System.IO
open Day5

[<EntryPoint>]
let main argv =
    File.ReadAllLines "../../Data/Day5.txt"
    |> available
    |> printfn "%A"
    0 // return an integer exit code
