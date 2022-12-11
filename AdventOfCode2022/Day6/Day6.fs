module Day6

open System.IO

module private TestData =
    let input = File.ReadAllText("Day6/input.txt")

let detect (input: string) (requiredDistincts: int) =
    input
    |> Seq.windowed requiredDistincts
    |> Seq.indexed
    |> Seq.map (fun (i, window) -> (i + requiredDistincts, window |> Set.ofSeq))
    |> Seq.find (fun (_, window) -> (window |> Set.count) = requiredDistincts)
    |> fst

module Puzzle11 = 
    let solve (input: string) =
        detect input 4

    let result = solve TestData.input

module Puzzle12 = 
    let solve (input: string) =
        detect input 14

    let result = solve TestData.input