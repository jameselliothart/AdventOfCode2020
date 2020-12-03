// Learn more about F# at http://fsharp.org

open System
open System.IO
open Day1
open Day2
open Day3

[<EntryPoint>]
let main argv =
    let entries = File.ReadAllLines "../../Data/Day3.txt"

    slopes
    |> Array.Parallel.map (traverse entries initialCount)
    |> Array.reduce (*)
    |> printfn "%i"
    0 // return an integer exit code
