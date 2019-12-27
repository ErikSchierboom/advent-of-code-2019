module AdventOfCode.Day4

let digits n =
    n
    |> Seq.unfold (fun x -> if x = 0 then None else Some(x % 10, x / 10))
    |> Seq.rev
    |> Seq.toList

let minimum = 235741
let maximum = 706948

let partOne =
    let isValid password =
        let passwordDigits = digits password
        let hasAdjacentPair =
            passwordDigits
            |> List.pairwise
            |> List.exists (fun (x, y) -> x = y)
        let digitsAreOrderedAscending =
            passwordDigits
            |> List.pairwise
            |> List.forall (fun (x, y) -> x <= y)
        
        hasAdjacentPair && digitsAreOrderedAscending
    
    [minimum..maximum]
    |> List.filter isValid
    |> List.length
    
let partTwo =
    let isValid password =
        let passwordDigits = digits password
        let hasAdjacentPair =
            List.countBy id passwordDigits
            |> List.map fst
            |> List.contains 2
        let digitsAreOrderedAscending =
            passwordDigits
            |> List.pairwise
            |> List.forall (fun (x, y) -> x <= y)
        
        hasAdjacentPair && digitsAreOrderedAscending
    
    [minimum..maximum]
    |> List.filter isValid
    |> List.length

 