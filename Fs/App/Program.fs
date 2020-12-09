// Learn more about F# at http://fsharp.org

open System
open System.IO
open Day9

[<EntryPoint>]
let main argv =
    File.ReadAllLines "../../Data/Day9.txt"
    |> findNonXmasNumber 25
    |> printfn "%A"
    0 // return an integer exit code
