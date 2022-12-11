module Day6

open System.IO

module private TestData =
    let input = File.ReadAllText("Day6/input.txt")

let detect (input: string) (requiredDistincts: int) =
    let isNotStart charGroup =
        let charSet = 
            charGroup
            |> Array.map snd
            |> Set.ofArray

        (charSet |> Set.count) < requiredDistincts

    let secondLast =
        input.ToCharArray()
        |> Array.mapi (fun i c -> (i + 1, c))
        |> Array.windowed requiredDistincts
        |> Array.takeWhile isNotStart
        |> Array.last
        |> Array.last
        |> fst

    secondLast + 1

module Puzzle11 = 
    let solve (input: string) =
        detect input 4

    let result = solve TestData.input

module Puzzle12 = 
    let solve (input: string) =
        detect input 14

    let result = solve TestData.input