module Day2

open System.IO

module private TestData = 
    let input = File.ReadAllLines("Day2/input.txt");

module Puzzle3 = 
    type HandShape = 
    | Rock
    | Paper
    | Scissors

    let scoreForShape hs =
        match hs with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    let scoreForOutcome (l, r) =
        match l, r with
        | Rock, Paper -> 6
        | Paper, Scissors -> 6
        | Scissors, Rock -> 6
        | (_, _) when l = r -> 3
        | _ -> 0

    let parse str =
        match str with
        | "X" | "A" -> Rock
        | "Y" | "B" -> Paper
        | "Z" | "C" -> Scissors
        | _ -> failwith "invalid str"

    let solve (input: string array) =
        let guide = 
            input
            |> Array.map (fun l -> l.Split(" "))
            |> Array.map (fun a -> (a[0] |> parse, a[1] |> parse))

        guide
        |> Array.map (fun g -> (g |> scoreForOutcome) + (g |> snd |> scoreForShape))
        |> Array.sum

    let result = solve TestData.input

module Puzzle4 = 
    let score ls rs =
        match ls, rs with
        | "A", "X" -> 3
        | "A", "Y" -> 4
        | "A", "Z" -> 8
        | "B", "X" -> 1
        | "B", "Y" -> 5
        | "B", "Z" -> 9
        | "C", "X" -> 2
        | "C", "Y" -> 6
        | "C", "Z" -> 7
        | _ -> failwith "invalid str"

    let solve (input: string array) = 
        input
        |> Array.map (fun l -> l.Split(" "))
        |> Array.map (fun l -> score l[0] l[1])
        |> Array.sum

    let result = solve TestData.input