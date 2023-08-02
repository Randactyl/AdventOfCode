namespace AdventOfCode
module Shared =
    let split (separators : string) (x : string) =
        x.Split(separators)
    
    let parseInput (input: string[]) =
        // concatenate the input with a unique character
        input
        |> String.concat "|"
        // split the string double occurrences of the unique character,
        // giving an array of strings of the form "123|456|789" where each index represents one elf sack
        |> split "||"
        // split each sack on the pipe character, giving an array of arrays of strings of the form "123", "456", "789"
        |> Array.map (split "|")
        |> Array.map (fun x -> x |> Seq.map int)
