module AdventOfCode.Program

open AdventOfCode.Data

let fuelRequired mass =
    if mass <= 0 then 0
    else mass / 3 - 2

let fuelRequiredRecursively mass =
    let rec helper total remainder =
        let fuel = fuelRequired remainder
        if fuel <= 0 then total
        else helper (total + fuel) fuel

    helper 0 mass

let solveOne = List.sumBy fuelRequired masses
let solveTwo = List.sumBy fuelRequiredRecursively masses

[<EntryPoint>]
let main _ =
    printfn "One: %A" solveOne
    printfn "Two: %A" solveTwo
    0
