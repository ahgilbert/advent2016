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

let allHashes = Seq.map hash allSeeds

let goodHashes = Seq.filter (fun (x : string) -> x.StartsWith "00000") allHashes

let p1 =
    goodHashes
    |> Seq.take 8
    |> Seq.map (Seq.skip 5 >> Seq.head >> string)
    |> String.concat ""

let main =
    Console.WriteLine p1