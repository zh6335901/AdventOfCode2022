module Day4

open System.IO

module private TestData = 
    let input = File.ReadAllLines("Day4/input.txt")

type Range = int * int
type Pair = Range * Range

let parse (line: string): Pair =
    let parseRange (str: string) =
        let rangeArr = str.Split("-")
        (int rangeArr[0], int rangeArr[1])

    let pairArr = line.Split(",")
    (parseRange pairArr[0], parseRange pairArr[1])

let countTrue (bs: bool array) =
    bs |> Array.sumBy (fun b -> if b then 1 else 0)

module Puzzle7 = 
    let fullyContains (pair: Pair) = 
        match pair with
        | (l1, r1), (l2, r2) when l1 >= l2 && r1 <= r2 -> true
        | (l1, r1), (l2, r2) when l2 >= l1 && r2 <= r1 -> true
        | _ -> false

    let solve (lines: string array) = 
        lines
        |> Array.map parse
        |> Array.map fullyContains
        |> countTrue

    let result = solve TestData.input

module Puzzle8 =
    let overlap (pair: Pair) =
        match pair with
        | (l1, r1), (l2, r2) when l1 > r2 || r1 < l2 -> false
        | _ -> true

    let solve (lines: string array) = 
        lines
        |> Array.map parse
        |> Array.map overlap
        |> countTrue

    let result = solve TestData.input