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
        let xs = 
            lines
            |> parseCmds
            |> Seq.collect (function
                | Noop  -> [0]
                | Add x -> [0; x])
            |> Seq.scan (fun state x -> state + x) 1
            |> Seq.take 240

        xs
        |> Seq.indexed
        |> Seq.map (
            fun (c, x) ->
                let pos = c % 40
                if abs (x - pos) <= 1 then "#" else "."
        )
        |> Seq.chunkBySize 40
        |> Seq.map (String.concat "")
        |> String.concat "\n"

    let result = solve TestData.input
