module Day1

open System
open System.Numerics
open System.Text.RegularExpressions

exception Bunk of string

type Dir = R | L
type Order = Dir * int

let castDir d =
    match d with
    | 'R' -> R
    | 'L' -> L
    | x -> raise <| Bunk("bad turn direction")

let readOrder (h :: ts) =
    let dir = castDir h
    let faith = ts |> System.String.Concat |> Int32.Parse
    (dir, faith)

let pivot dir (lat,long) =
    match dir with
    | R -> if lat = 0 then (-long, 0) else (0, lat)
    | L -> if lat = 0 then (long, 0) else (0, -lat) 

let main =
    let input = System.IO.File.ReadLines(@"/home/alan/hdd/code/aadvent/input/01.txt")
    let orders = Seq.map readOrder (Seq.map Seq.toList input)
    Console.WriteLine(Seq.length input)