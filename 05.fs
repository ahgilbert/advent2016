[<AutoOpen>]
module D5

open System
open System.Security.Cryptography

let allSeeds =
    Seq.unfold (fun n -> Some("reyedfim" + string n, n + 1I)) 0I

let hash (input : string) =
    use md5 = MD5.Create()
    input
    |> Text.Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Array.map (fun (x : byte) -> System.String.Format("{0:X2}", x))
    |> String.concat ""

let allHashes =
    Seq.map hash allSeeds
    |> Seq.cache

let goodHashes =
    Seq.filter (fun (x : string) -> x.StartsWith "00000") allHashes

let p1 () =
    goodHashes
    |> Seq.take 8
    |> Seq.map (Seq.skip 5 >> Seq.head >> string)
    |> String.concat ""

let rec checkHash (hs : seq<string>) soFar =
    let allDone m =
        Seq.map (fun k -> Map.containsKey k m) [0..7]
        |> Seq.fold (&&) true
    let h = Seq.head hs
    let iChar = Seq.item 6 h
    let i = if Seq.contains iChar "01234567"
            then iChar |> string |> Int32.Parse
            else 999
    let c = Seq.item 7 h
    if i > 7 then checkHash (Seq.tail hs) soFar
    elif Map.containsKey i soFar then checkHash (Seq.tail hs) soFar
    else
        printf "%i : %c | " i c
        if allDone soFar then soFar
        else checkHash (Seq.tail hs) (Map.add i c soFar)

let p2 =
    checkHash goodHashes Map.empty

