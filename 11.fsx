type Machine = Chip of string | Gen of string
type Scene = int * list<list<Machine>>

let testInput =
    (0, [[Chip "H"; Chip "Li"]; [Gen "H"]; [Gen "Li"]; []])

let input = 
    (0,
     [[Gen "Po"; Gen "Tm"; Chip "Tm"; Gen "Pm"; Gen "Ru"; Chip "Ru"; Gen "Co"; Chip "Co"];
      [Chip "Po"; Chip "Pm"];
      [];
      []])

let all = List.fold (&&) true

let rec getPairs xs =
    if (List.length xs < 2) then []
    else
        let h = List.head xs
        let headPairs = List.map (fun x -> [h; x]) (List.tail xs)
        List.append headPairs (getPairs (List.tail xs))

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
    && List.map (explodes >> not) distribution
    |> all

let move dir cargo (currentFloor, floors) =
// update game state
    let destFloor = currentFloor + dir
    let staysPut = fun m -> not <| List.contains m cargo
    if destFloor > 4 || destFloor < 0
    then (-1, floors) // force illegal game state, prune this move
    else
        let thisFloor' = List.filter staysPut (List.item currentFloor floors)
        let newFloor' = List.append cargo (List.item destFloor floors)
        let floorDoer = fun i ->
            if i = currentFloor then thisFloor'
            elif i = destFloor then newFloor'
            else List.item i floors
        let newFloors = List.map floorDoer [0..3] // Replace new floor and old floor...
        (destFloor, newFloors)

let generateMoves (currentFloor, floors) =
    let directions =
        if currentFloor = 0 then [1]
        elif currentFloor = 3 then [-1]
        else [1;-1]
    let stuffHere = List.item currentFloor floors
    let singleCargo = List.map (fun x -> [x]) stuffHere
    let possibleCargo = List.append singleCargo (getPairs stuffHere)
    [for dir in directions do
     for cargo in possibleCargo do
     yield (dir, cargo, (currentFloor, floors))]
    |> Seq.toList