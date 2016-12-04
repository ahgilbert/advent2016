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
    | R -> if lat = 0
           then (-long, 0)
           else (0, lat)
    | L -> if lat = 0
           then (long, 0)
           else (0, -lat) 

let scalarMultiply (x,y) z =
    (x*z, y*z)

let addTuples (a,b) (c,d) =
    (a+c, b+d)

let step dir dist head loc =
    let newDir = pivot dir head
    let scaled = scalarMultiply newDir dist
    (newDir, addTuples loc scaled)

let followOneOrder (heading, location) (dir, dist) =
    step dir dist heading location

let main =
    let input = Seq.cache <| System.IO.File.ReadLines(@"/home/alan/hdd/code/aadvent/input/01.txt")
    let orders = Seq.map readOrder (Seq.map Seq.toList input)
    let (_, (lat,long)) = Seq.fold followOneOrder ((1,0),(0,0)) orders
    Console.WriteLine("Lat: " + string lat + " Long: " + string long)
    Console.WriteLine("Santa will travel " + string (Operators.abs lat + Operators.abs long) + " blocks")