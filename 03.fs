[<AutoOpen>]
module D3

open System
open System.IO

let readLines path =
    File.ReadAllLines path

let split (delimiter:string) (input:string) =
    input.Split([|delimiter|], StringSplitOptions.RemoveEmptyEntries)

let input =
    let lines = readLines "input/03.txt"
    lines
    |> Seq.map (split " " >> (Seq.map int) >> Seq.toList)
    |> Seq.toList

let legit [x;y;z] =
    x + y > z && y + z > x && x + z > y

let part1 =
    input
    |> Seq.filter legit
    |> Seq.length

let part2 =
    let input' =
        let rec loop xs = seq {
            match xs with
            | [] -> ()
            | [x;y;z]::[x';y';z']::[x'';y'';z'';]::xs' ->
                yield! [[x;x';x''];[y;y';y''];[z;z';z'']]
                yield! loop xs' }
        loop input
    input'
    |> Seq.filter legit
    |> Seq.length

let main =
    Console.WriteLine part1
    Console.WriteLine part2