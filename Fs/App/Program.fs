// Learn more about F# at http://fsharp.org

open System
open System.IO
open Day7

[<EntryPoint>]
let main argv =
    File.ReadAllLines "../../Data/Day7.txt"
    |> bagMustContain "shiny gold"
    |> printfn "%A"
    0 // return an integer exit code
