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
    |> Seq.groupBy (fun (_,x) -> Seq.length x)
    |> Seq.sortBy fst
    |> Seq.rev
    |> Seq.map snd
    |> Seq.map (Seq.map fst)
    |> Seq.map Seq.sort
    |> Seq.concat
    |> Seq.take 5
    |> Seq.map string
    |> String.concat ""

let checkEntry (name, sector, checksum) =
    checksum = genChecksum name

let readLines day =
    "input/" + (string day).PadLeft (2,'0') + ".txt"
    |> File.ReadLines 

let input = Seq.cache <| readLines 4

let main =
    input
    |> Seq.map readRoom
    |> Seq.filter checkEntry
    |> Seq.map (fun (_,x,_) -> x)
    |> Seq.length
    |> Console.WriteLine
