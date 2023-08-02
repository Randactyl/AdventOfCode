module RockPaperScissors =
    type Move =
        | Rock
        | Paper
        | Scissors

    type RoundResult =
        | Win
        | Loss
        | Tie

    let JudgeGame player1Move player2Move =
        match player1Move, player2Move with
        | Rock, Scissors -> Win, Loss
        | Rock, Paper -> Loss, Win
        | Rock, Rock -> Tie, Tie
        | Paper, Rock -> Win, Loss
        | Paper, Scissors -> Loss, Win
        | Paper, Paper -> Tie, Tie
        | Scissors, Paper -> Win, Loss
        | Scissors, Rock -> Loss, Win
        | Scissors, Scissors -> Tie, Tie

    let ResultForOpponent myResult =
        match myResult with
        | Win -> Loss
        | Loss -> Win
        | Tie -> Tie

module ElfinRockPaperScissors =
    open RockPaperScissors

    let PlayerScore playerMove playerOutcome =
        // (move score) + (outcome score)
        (match playerMove with
         | Rock -> 1
         | Paper -> 2
         | Scissors -> 3)
        + (match playerOutcome with
           | Loss -> 0
           | Tie -> 3
           | Win -> 6)

module Part1 =
    open RockPaperScissors
    open ElfinRockPaperScissors

    let private mapToMove encrypted_move =
        match encrypted_move with
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissors
        | "X" -> Rock
        | "Y" -> Paper
        | "Z" -> Scissors
        | _ -> failwithf "This case should never be reached. encrypted_move: %A" encrypted_move

    let private scoreGame (encrypted_game: string) =
        let strategyParts = encrypted_game.Split(' ')
        let opponentMove = mapToMove strategyParts[0]
        let myMove = mapToMove strategyParts[1]
        let opponentOutcome, myOutcome = JudgeGame opponentMove myMove

        (PlayerScore opponentMove opponentOutcome), (PlayerScore myMove myOutcome)

    let Run (input: string[]) =
        input
        |> Array.map (fun x ->
            match scoreGame x with
            | _, playerScore -> playerScore)
        |> Array.sum

module Part2 =
    open RockPaperScissors
    open ElfinRockPaperScissors

    let private decryptOpponentMove encrypted_move =
        match encrypted_move with
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissors
        | _ -> failwithf "This case should never be reached. encrypted_move: %A" encrypted_move

    let private decryptOutcome encrypted_outcome =
        match encrypted_outcome with
        | "X" -> Loss
        | "Y" -> Tie
        | "Z" -> Win
        | _ -> failwithf "This case should never be reached. encrypted_outcome: %A" encrypted_outcome

    let private chooseMove opponentMove desiredResult =
        match opponentMove, desiredResult with
        | Rock, Win -> Paper
        | Rock, Loss -> Scissors
        | Rock, Tie -> Rock
        | Paper, Win -> Scissors
        | Paper, Loss -> Rock
        | Paper, Tie -> Paper
        | Scissors, Win -> Rock
        | Scissors, Loss -> Paper
        | Scissors, Tie -> Scissors

    let private scoreGame (encrypted_game: string) =
        let strategyParts = encrypted_game.Split(' ')
        let opponentMove = decryptOpponentMove strategyParts[0]
        let desiredResult = decryptOutcome strategyParts[1]
        let myMove = chooseMove opponentMove desiredResult

        (PlayerScore opponentMove (ResultForOpponent desiredResult)), (PlayerScore myMove desiredResult)


    let Run input =
        input
        |> Array.map (fun x ->
            match scoreGame x with
            | _, playerScore -> playerScore)
        |> Array.sum

let () =
    let test_input = [| "A Y"; "B X"; "C Z" |]
    test_input |> Part1.Run |> printfn "Test 1: %A"
    assert (Part1.Run test_input = 15)
    test_input |> Part2.Run |> printfn "Test 2: %A"
    assert (Part2.Run test_input = 12)

    let input = System.IO.File.ReadAllLines "input.txt"
    input |> Part1.Run |> printfn "Part 1: %A"
    input |> Part2.Run |> printfn "Part 2: %A"
