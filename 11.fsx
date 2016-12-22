type Machine = Chip of string | Gen of string
type Scene = int * list<list<Machine>>
type Move = U | D

let testInput =
    (0, [[Chip "H"; Chip "Li"]; [Gen "H"]; [Gen "Li"]; []])

let input = 
    (0,
     [[Gen "Po"; Gen "Tm"; Chip "Tm"; Gen "Pm"; Gen "Ru"; Chip "Ru"; Gen "Co"; Chip "Co"];
      [Chip "Po"; Chip "Pm"];
      [];
      []])

let all = List.fold (&&) true

let checkVictory (_, scene) =
    List.take 3 scene
    |> List.map List.isEmpty
    |> all

let isPaired a b =
    match a with
    | (Chip x) -> List.contains (Gen x) b
    | (Gen x) -> List.contains (Chip x) b

// No unpaired chip and unpaired generator
let explodes equipment =
    let hasChip ms = List.exists (fun x -> match x with | Chip _ -> true | _ -> false) ms
    let hasGen ms = List.exists (fun x -> match x with | Gen _ -> true | _ -> false) ms
    let isChip = (fun x -> 
        match x with 
        | Chip _ -> true
        | _ -> false )
    let unpaired = List.filter ((fun x -> isPaired x equipment) >> not) equipment
    hasChip unpaired && hasGen unpaired

let isLegalState (level, distribution) =
    level >= 0 
    && level <= 4
    && List.map (explodes >> not) distribution |> all