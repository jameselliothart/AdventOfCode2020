// Learn more about F# at http://fsharp.org

open System
open System.IO
open Day5

[<EntryPoint>]
let main argv =
    data
    |> maxSeatId
    |> printfn "%i"
    0 // return an integer exit code
