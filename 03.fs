[<AutoOpen>]
module D3

open System

let readInput =
    let faith (s : string) = s.Split ' ' |> Seq.filter (fun x -> x <> "") 
                             |> Seq.map Int32.Parse
                             |> fun is -> (Seq.item 0 is, Seq.item 1 is, Seq.item 2 is)
    System.IO.File.ReadLines "input/03.txt"
    |> Seq.map faith

let legit ((a,b,c) : (int * int * int)) =
    let sorted = Seq.sort [a;b;c] |> Seq.rev
    let big = Seq.head sorted
    let rest = Seq.tail sorted
    Seq.sum rest > big

let main =
    let input = Seq.cache readInput
    let part1 = input |> Seq.filter legit
                      |> Seq.length
    Console.WriteLine ("part 1 shows " + string part1 + " illegal triangles")