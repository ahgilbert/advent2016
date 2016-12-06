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
    let newLocation = addTuples loc scaled
    (newDir, newLocation)

let followOneOrder (heading, location) (dir, dist) =
    step dir dist heading location

let rec checkMatch xs =
    if Seq.isEmpty xs
    then raise <| Bunk "no duplicates"
    else let here = Seq.head xs
         let later = Seq.tail xs
         if Seq.exists (fun x -> x = here) later
         then here
         else checkMatch later

let connectPoints ((x1,y1),(x2,y2)) =
    match (x1 = x2) with
    | true -> let steps = Seq.map (fun y -> (x1,y)) [min y1 y2..max y1 y2]
              if y1 < y2
              then Seq.tail steps
              else Seq.tail <| Seq.rev steps
    | false -> let steps = Seq.map (fun x -> (x,y1)) [min x1 x2..max x1 x2]
               if x1 < x2
               then Seq.tail steps
               else Seq.tail <| Seq.rev steps

let interpolate xs =
    let legs = Seq.pairwise xs
    let paces = Seq.concat <| Seq.map connectPoints legs
    Seq.append [Seq.head xs] paces

let readInput =
    let input = Seq.cache <| System.IO.File.ReadLines(@"/home/alan/hdd/code/aadvent/input/01.txt")
    Seq.map readOrder (Seq.map Seq.toList input)

let dist (x1,y1) (x2,y2) =
    let dx = abs (x1 - x2)
    let dy = abs (y1 - y2)
    dx + dy

let main =
    let orders = readInput
    let landmarks = Seq.map snd <| Seq.scan followOneOrder ((1,0),(0,0)) orders
    let final = Seq.last landmarks
    Console.WriteLine("Santa will travel " + string (dist (0,0) final) + " blocks")
    let wholePath = interpolate landmarks
    let firstDupe = checkMatch wholePath
    Console.WriteLine("first duplicate: " + string (dist (0,0) firstDupe) + " away")