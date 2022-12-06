module Day1

open System.IO

module private TestData = 
    let input = File.ReadAllText("Day1/input.txt")
    let calories = 
        input.Split("\r\n\r\n")
        |> Array.map (fun g -> g.Split("\r\n") |> Array.map int)

module Puzzle1 = 
    let solve (calories: int array array): int = 
        calories
        |> Array.map (fun g -> g |> Array.sum)
        |> Array.max

    let result = solve TestData.calories

module Puzzle2 = 
    let solve (calories: int array array): int =
        calories
        |> Array.map (fun g -> g |> Array.sum)
        |> Array.sortDescending
        |> Array.take 3
        |> Array.sum

    let result = solve TestData.calories