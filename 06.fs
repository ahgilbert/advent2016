[<AutoOpen>]
module D6

open System
open System.IO

let input =
    File.ReadLines "input/06.txt"
    |> Seq.cache

let grabMostCommon xs =
    Seq.groupBy id xs
    |> Seq.sortBy (fun (_,ys) -> Seq.length ys)
    |> Seq.rev
    |> Seq.head
    |> fst

let grabLeastCommon xs =
    Seq.groupBy id xs
    |> Seq.sortBy (fun (_,ys) -> Seq.length ys)
    |> Seq.head
    |> fst


let p6 grabber =
    let indexes = [0..Seq.length (Seq.head input) - 1]
    let columns = Seq.map (fun i -> Seq.map (Seq.item i) input) indexes
    Seq.map grabber columns
    |> Seq.map string
    |> String.concat ""

let p6a = p6 grabMostCommon
let p6b = p6 grabLeastCommon

let main =
    Console.WriteLine p6a
    Console.WriteLine p6b