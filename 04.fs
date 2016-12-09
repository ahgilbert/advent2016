[<AutoOpen>]
module D4

open System.IO

let readLines day =
    "input/" + (string day).PadLeft (2,'0') + ".txt"
    |> File.ReadLines 

let input = readLines 4
