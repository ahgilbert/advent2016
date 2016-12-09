module Prelude

open System
open System.IO

let readLines day =
    "input/" + (string day).PadLeft (2,'0') + ".txt"
    |> File.ReadLines 