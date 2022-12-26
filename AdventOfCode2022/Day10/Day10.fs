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
        let cycles = 
            lines
            |> parseCmds 
            |> Seq.collect (function
                | Noop  -> [0]
                | Add x -> [0; x])
            |> Seq.take 220
            |> Seq.scan (fun state x -> state + x) 1

        [ 20; 60; 100; 140; 180; 220 ]
        |> List.map (fun c -> c, cycles |> Seq.item (c - 1))
        |> List.map (fun t -> t ||> (*))
        |> List.sum
    
    let result = solve TestData.input