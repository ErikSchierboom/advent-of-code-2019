module AdventOfCode.Day2

let intcode() =
    [| 1
       0
       0
       3
       1
       1
       2
       3
       1
       3
       4
       3
       1
       5
       0
       3
       2
       1
       9
       19
       1
       19
       5
       23
       2
       6
       23
       27
       1
       6
       27
       31
       2
       31
       9
       35
       1
       35
       6
       39
       1
       10
       39
       43
       2
       9
       43
       47
       1
       5
       47
       51
       2
       51
       6
       55
       1
       5
       55
       59
       2
       13
       59
       63
       1
       63
       5
       67
       2
       67
       13
       71
       1
       71
       9
       75
       1
       75
       6
       79
       2
       79
       6
       83
       1
       83
       5
       87
       2
       87
       9
       91
       2
       9
       91
       95
       1
       5
       95
       99
       2
       99
       13
       103
       1
       103
       5
       107
       1
       2
       107
       111
       1
       111
       5
       0
       99
       2
       14
       0
       0 |]

let restoreState state =
    Array.set state 1 12
    Array.set state 2 2
    state

let rec processOperations index state =
    let opcode = Array.get state (index + 0)
    let srcPos1 = Array.get state (index + 1)
    let srcPos2 = Array.get state (index + 2)
    let destPos = Array.get state (index + 3)

    if opcode = 1 then
        state.[destPos] <- state.[srcPos1] + state.[srcPos2]
        processOperations (index + 4) state
    elif opcode = 2 then
        state.[destPos] <- state.[srcPos1] * state.[srcPos2]
        processOperations (index + 4) state
    else
        state

let partOne =
    let state = intcode()

    let endState =
        state
        |> restoreState
        |> processOperations 0

    Array.get endState 0

let partTwo =

    let solve (x, y) =
        let state = intcode()
        Array.set state 1 x
        Array.set state 2 y

        state
        |> processOperations 0

    let isTargetSolution (x, y) =
        let state = solve (x, y)
        Array.get state 0 = 19690720
    
    [ for x in 0 .. 99 do
        for y in 0 .. 99 do
            (x, y) ]
    |> List.find isTargetSolution
 