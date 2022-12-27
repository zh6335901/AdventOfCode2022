module Day10

open System.IO

module private TestData = 
    let input = File.ReadAllLines("Day10/input.txt")

type Cmd = 
    | Noop
    | Add of int

let parseCmds (lines: string array) = 
    lines
    |> Array.map (function
        | "noop" -> Noop
        | x -> let a = x.Split(" ") in Add (int a[1])
    )

module Puzzle19 = 
    let solve lines = 
        lines
        |> parseCmds 
        |> Seq.collect (function
            | Noop  -> [(1, 0)]
            | Add x -> [(1, 0); (1, x)])
        |> Seq.take 220
        |> Seq.scan (fun state (cycle, x) -> (fst state + cycle, snd state + x)) (0, 1)
        |> Seq.filter (fun (i, _) -> (i - 19) % 40 = 0) // during (20 + x * 40)th cycle, not after
        |> Seq.map (fun (cycle, x) -> (cycle + 1) * x)
        |> Seq.sum
    
    let result = solve TestData.input

module Puzzle20 = 
    let solve lines =
        let positions = 
            lines
            |> parseCmds
            |> Seq.collect (function
                | Noop  -> [0]
                | Add x -> [0; x])
            |> Seq.scan (fun state x -> state + x) 1
            |> Seq.toArray

        let draw state cycle = 
            let pos = Array.item cycle positions
            let x = cycle % 40
            let state' = state + if x = 0 then "\n" else ""

            state' + if abs (pos - x) <= 1 then "#" else "."

        [0..239]
        |> Seq.fold draw ""

    let result = solve TestData.input
