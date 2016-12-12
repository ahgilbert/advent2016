[<AutoOpen>]
module D4

open System
open System.IO
open System.Text.RegularExpressions

let roomRegex = Regex("([a-z-]+)-(\d+)\[([a-z]+)\]$")

let readRoom str =
    let faith = roomRegex.Match str
    let name = faith.Groups.[1].Value
    let sector = Int32.Parse (faith.Groups.[2].Value)
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

let c2nMap =
    Seq.zip "abcdefghijklmnopqrstuvwxyz" [0..99]
    |> Map.ofSeq

let n2cMap =
    Seq.zip [0..99] "abcdefghijklmnopqrstuvwxyz"
    |> Map.ofSeq

let charToNumber c =
    Map.find c c2nMap

let numberToChar n =
    Map.find n n2cMap

let shiftCipher n c =
    let n' = n % 26
    (n' + charToNumber c) % 26
    |> numberToChar

let decode name sector =
    Seq.map (fun c ->
                 match c with
                 | '-' -> ' '
                 |  _  -> shiftCipher sector c)
            name
    |> Seq.map string
    |> String.concat ""

let main =
    let legitEntries =
        input
        |> Seq.map readRoom
        |> Seq.filter checkEntry
    let sumOfSectors =
        legitEntries
        |> Seq.map (fun (_,x,_) -> x)
        |> Seq.sum
    Console.WriteLine sumOfSectors
    let trueNames =
        legitEntries
        |> Seq.map (fun (n,s,_) -> (decode n s, s))
        |> Seq.toList
    ignore <| List.map (printf "%A\n") trueNames