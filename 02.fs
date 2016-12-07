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

let numPad1 =
    [ for a in 1..3 do
      for b in 1..3 do
          yield (a,b) ]
    |> (fun xs -> Seq.zip xs "123456789")
    |> Set.ofSeq

let numPad2 =
    Seq.concat [[(3,1)];
                [ for x in 2..4 do yield (x,2) ];
                [ for x in 1..5 do yield (x,3) ];
                [ for x in 2..4 do yield (x,4) ];
                [(3,5)]]
    |> (fun xs -> Set.zip xs "123456789ABCD")
    |> Set.ofSeq

let boundedMove validKeys (x,y) d =
    let newLoc =
        match d with
        | Right -> (x + 1, y)
        | Left -> (x - 1, y)
        | Up -> (x, y-1)
        | Down -> (x, y+1)
    if Set.contains newLoc validKeys
    then newLoc
    else (x,y)

let getDigit (x,y) =
    x + ((y - 1) * keypad_size)
    
let getNextKey keypad start orders =
    Seq.fold (boundedMove keypad) start orders

let readInput file =
    Seq.cache <| System.IO.File.ReadLines(file)
    |> Seq.map Seq.toList

let day2a file =
    readInput file
    |> Seq.map (Seq.map readDirection)
    |> Seq.scan (getNextKey (map fst numPad1)) (2,2)
    |> Seq.tail
    |> Seq.map getDigit
    |> Seq.map string
    |> Seq.map Console.WriteLine
    
let main = day2a @"/home/alan/hdd/code/aadvent/input/02.txt"
