open AdventOfCode.Shared

let Part1 (input: string[]) =
    parseInput input
    |> Array.map (fun sequenceOfIts -> sequenceOfIts |> Seq.sum)
    |> Array.max

let Part2 (input: string[]) =
    parseInput input
    |> Array.map (fun sequenceOfIts -> sequenceOfIts |> Seq.sum)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum

let () =
    let input = System.IO.File.ReadAllLines "input.txt"
    
    input
    |> Part1
    |> printfn "Part 1: %d"

    input
    |> Part2
    |> printfn "Part 2: %A"
