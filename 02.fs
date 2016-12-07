module D2

open System

type Dir = Right | Left | Up | Down

let keypad_size = 3

exception Bunk of string

let readDirection c =
    match c with
    | 'R' -> Right
    | 'L' -> Left
    | 'U' -> Up
    | 'D' -> Down
    |  _  -> raise <| Bunk "bad direction"

let boundedMove (x,y) d =
    let squeeze n = max 1 <| min keypad_size n
    let squeeze' (a,b) = (squeeze a, squeeze b)
    match d with
    | Right -> squeeze' (x + 1, y)
    | Left -> squeeze' (x - 1, y)
    | Up -> squeeze' (x, y-1)
    | Down -> squeeze' (x, y+1)

let getDigit (x,y) =
    x + ((y - 1) * keypad_size)
    
let faith start orders =
    Seq.fold boundedMove start orders

let readInput file =
    Seq.cache <| System.IO.File.ReadLines(file)
    |> Seq.map Seq.toList

let day2 file =
    readInput file
    |> Seq.map (Seq.map readDirection)
    |> Seq.scan faith (2,2)
    |> Seq.tail
    |> Seq.map getDigit
    |> Seq.map string
    |> Seq.map Console.WriteLine
    
let main = day2 @"/home/alan/hdd/code/aadvent/input/02.txt"