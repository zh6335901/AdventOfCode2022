module Day3

open System.IO

module private TestData = 
    let input = File.ReadAllLines("Day3/input.txt");

let getPriority (c: char) =
    if System.Char.IsUpper c 
    then (int c) - (int 'A') + 27
    else (int c) - (int 'a') + 1

let strToCharSet (str: string) =
    str.ToCharArray() |> Set.ofArray

module Puzzle5 = 
    let solve (lines: string array) = 
        lines
        |> Array.map (fun l -> let cp = l.Length / 2 in (l.Substring(0, cp), l.Substring(cp)))
        |> Array.map (fun a -> strToCharSet (fst a), strToCharSet (snd a))
        |> Array.map (fun x -> (fst x) |> Set.intersect (snd x))
        |> Array.map (fun s -> s |> Set.toSeq |> Seq.sumBy getPriority)
        |> Array.sum

    let result = solve TestData.input

module Puzzle6 = 
    let solve (lines: string array) =
        lines
        |> Array.chunkBySize 3
        |> Array.map (fun g -> g |> Array.map strToCharSet)        
        |> Array.map (fun x -> x |> Array.reduce (fun s i -> Set.intersect s i))
        |> Array.map (fun s -> s |> Set.toSeq |> Seq.sumBy getPriority)
        |> Array.sum

    let result = solve TestData.input
