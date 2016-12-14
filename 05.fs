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

let p2 =
    let mutable (slots : Map<int, char>) = Map.empty
    let checkHash (h : string) =
        let i = Seq.item 6 h |> string |> Int32.Parse
        let c = Seq.item 7 h
        if i > 7 then false
        elif Map.containsKey i slots then false
        else
            printf "%i : %c" i c
            slots <- Map.add i c slots
            true
    goodHashes
    |> Seq.filter checkHash
    |> Seq.take 8
    |> ignore
    slots

