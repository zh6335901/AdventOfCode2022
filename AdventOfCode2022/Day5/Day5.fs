module Day5

open System.IO
open System.Text.RegularExpressions

type Stack = char list
type MoveCommand = int * int * int

module private TestData =
    let input = File.ReadAllText("Day5/input.txt")

let parse (input: string): (Stack array * MoveCommand array) = 
    let parseStacks (str: string): Stack array = 
        let lines = str.Split("\r\n")
        let stacks: Stack array = Array.init (lines[0].Length / 4 + 1) (fun _ -> [])

        for line in lines do
            let cs = 
                line.ToCharArray() 
                |> Array.mapi (fun i c -> (c, i))
                |> Array.filter (fun (_, i) -> (i - 1) % 4 = 0)
            
            for c, i in cs do
                if System.Char.IsLetter c then
                    stacks[i / 4] <- stacks[i / 4] @ [c]

        stacks
            
    let parseCommands (str: string): (MoveCommand array) = 
        str.Split("\r\n")
        |> Array.map (fun l -> seq { for m in Regex.Matches(l, "\d+") do yield (int m.Value) })
        |> Array.map (fun s -> let a = s |> Array.ofSeq in (a[0], a[1], a[2]))

    let parts = input.Split("\r\n\r\n")
    (parts[0] |> parseStacks, parts[1] |> parseCommands)

module Puzzle9 = 
    let move (stacks: Stack array) (command: MoveCommand) =
        let c, fi, ti = command
        let from = stacks[fi - 1]
        let to' = stacks[ti - 1]

        stacks[fi - 1] <- from[c..]
        stacks[ti - 1] <- (from |> List.take c |> List.rev) @ to'

        stacks

    let solve input = 
        let stacks, commands = parse input
        let final = commands |> Array.fold move stacks

        final 
        |> Array.map (fun s -> s |> List.head |> string)
        |> String.concat ""

    let result = solve TestData.input

module Puzzle10 = 
    let move (stacks: Stack array) (command: MoveCommand) =
        let c, fi, ti = command
        let from = stacks[fi - 1]
        let to' = stacks[ti - 1]

        stacks[fi - 1] <- from[c..]
        stacks[ti - 1] <- (from |> List.take c) @ to'

        stacks

    let solve input = 
        let stacks, commands = parse input
        let final = commands |> Array.fold move stacks

        final 
        |> Array.map (fun s -> s |> List.head |> string)
        |> String.concat ""

    let result = solve TestData.input