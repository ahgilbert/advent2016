[<AutoOpen>]
module D4

open System
open System.IO
open System.Text.RegularExpressions

let roomRegex = Regex("([a-z-]+)-(\d+)\[([a-z]+)\]$")

let readRoom str =
    let faith = roomRegex.Match str
    let name = faith.Groups.[1].Value
    let sector = faith.Groups.[2].Value
    let checksum = faith.Groups.[3].Value
    (name, sector, checksum)

let genChecksum name =
    Seq.filter (fun c -> c <> '-') name
    |> Seq.groupBy id
    |> Seq.sortBy (fun (_,x) -> 0 - Seq.length x)
    |> Seq.take 5
    |> Seq.map fst
    |> Seq.map string
    |> String.concat ""
    |> Console.WriteLine

let readLines day =
    "input/" + (string day).PadLeft (2,'0') + ".txt"
    |> File.ReadLines 

let input = readLines 4

let main =
    input
    |> Seq.map readRoom
    |> Seq.head
    |> (fun (x,_,_) -> x)
    |> genChecksum
    |> Console.WriteLine
