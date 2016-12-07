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

let numPad1 : Map<(int * int), char> =
    [ for a in 1..3 do
      for b in 1..3 do
          yield (a,b) ]
    |> (fun xs -> Seq.zip xs "123456789")
    |> Map.ofSeq

let numPad2 =
    Seq.concat [[(3,1)];
                [ for x in 2..4 do yield (x,2) ];
                [ for x in 1..5 do yield (x,3) ];
                [ for x in 2..4 do yield (x,4) ];
                [(3,5)]]
    |> (fun xs -> Seq.zip xs "123456789ABCD")
    |> Map.ofSeq

let boundedMove validKeys (x,y) d =
    let newLoc =
        match d with
        | Right -> (x + 1, y)
        | Left -> (x - 1, y)
        | Up -> (x, y-1)
        | Down -> (x, y+1)
    if Map.containsKey newLoc validKeys
    then newLoc
    else (x,y)

let getButton numPad k =
    Map.find k numPad
    
let getNextKey (keypad : Map<(int * int), char>) start orders =
    Seq.fold (boundedMove keypad) start orders

let readInput file =
    Seq.cache <| System.IO.File.ReadLines(file)
    |> Seq.map Seq.toList
    |> Seq.map (Seq.map readDirection)

let day2 numPad start input =
    Seq.scan (getNextKey numPad) start input
    |> Seq.tail
    |> Seq.map (getButton numPad)
    |> String.Concat

let main =
    let input = readInput "input/02.txt"
    let d2a = day2 numPad1 (2,2) input
    let d2b = day2 numPad2 (3,1) input
    Console.WriteLine "how do I display these values?"
