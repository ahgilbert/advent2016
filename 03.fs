[<AutoOpen>]
module D3

open System

let readInput = System.IO.File.ReadLines "input/03.txt"

let seqToTriplet threes =
    (Seq.item 0 threes, Seq.item 1 threes, Seq.item 2 threes)

let parseInputA input =
    let faith (s : string) = s.Split ' '
                             |> Seq.filter (fun x -> x <> "") 
                             |> Seq.map Int32.Parse
                             |> seqToTriplet
    Seq.map faith input

let legit ((a,b,c) : (int * int * int)) =
    let sorted = Seq.sort [a;b;c] |> Seq.rev
    let big = Seq.head sorted
    let rest = Seq.tail sorted
    Seq.sum rest > big

let main =
    let input = Seq.cache readInput
    let inputA = parseInputA input
    let part1 = inputA |> Seq.filter legit
                       |> Seq.length
    Console.WriteLine ("part 1 shows " + string part1 + " illegal triangles")