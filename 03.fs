[<AutoOpen>]
module D3

open System

let sideways xs =
    let xs = Seq.map (fun a -> Seq.item 0 a) xs
    let ys = Seq.map (fun a -> Seq.item 1 a) xs
    let zs = Seq.map (fun a -> Seq.item 2 a) xs
    Seq.concat [ xs; ys; zs; ]

let seqToTriplet threes =
    (Seq.item 0 threes, Seq.item 1 threes, Seq.item 2 threes)

let readInput =
    let faith (s : string) = s.Split ' '
                             |> Seq.filter (fun x -> x <> "") 
                             |> Seq.map Int32.Parse
    Seq.cache <| System.IO.File.ReadLines "input/03.txt"
    |> Seq.map faith

let parseInputA input =
    Seq.map seqToTriplet input

let rec chunk n xs =
    match Seq.isEmpty xs with
    | true -> seq []
    | false ->
      let zz = Seq.take n xs
      let yy = Seq.skip n xs
      Seq.append (seq [zz]) (chunk n yy) 

let parseInputB input =
    let a = sideways input
    "ahg"

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