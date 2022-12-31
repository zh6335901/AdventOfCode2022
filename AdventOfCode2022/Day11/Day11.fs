module Day11

open System.IO

module private TestData =
    let input = File.ReadAllText("Day11/input.txt")

type Operation = int64 -> int64
type Throw = int64 -> int
type Monkey = { Items: int64 list; Operation: Operation; DivisibleBy: int64; Throw: Throw }

let split (sep: string) (str: string) = str.Split(sep)

let parseMonkeys (input: string) = 
    let monkeyDescriptions = input |> split "\r\n\r\n"
    monkeyDescriptions
    |> Array.map (fun d -> d |> split "\r\n" |> Array.tail)
    |> Array.map (
        fun ls ->
            let items = 
                ls[0] 
                |> split ": " 
                |> Array.last
                |> split ", " 
                |> Array.map int64
                |> Array.toList

            let operation =
                match ls[1] |> split " = " |> Array.last with
                | "old * old" -> (fun x -> x * x)
                | "old + old" -> (fun x -> x + x)
                | expr when expr.Contains "*" -> let multiplier = expr |> split " * " |> Array.last |> int64 in (fun x -> x * multiplier)
                | expr when expr.Contains "+" -> let addend = expr |> split " + " |> Array.last |> int64 in (fun x -> x + addend)
                | _ -> failwith "Not supported"
            
            let divisibleBy = ls[2] |> split " " |> Array.last |> int64
            let throwToIfTrue = ls[3] |> split " " |> Array.last |> int
            let throwToIfFalse = ls[4] |> split " " |> Array.last |> int
            let throw: (int64 -> int) = (fun x -> if x % divisibleBy = 0 then throwToIfTrue else throwToIfFalse)

            { Items = items; Operation = operation; DivisibleBy = divisibleBy; Throw = throw }
    )

let round roundCount relieve (monkeys: Monkey array) = 
    let rec doThrow i times = 
        let monkey = monkeys[i]
        let sorted = monkey.Items |> List.sort
        match sorted with
        | level :: tail -> 
            let newLevel = level |> monkey.Operation |> relieve
            let throwTo = newLevel |> monkey.Throw
            monkeys[throwTo] <- { monkeys[throwTo] with Items = newLevel :: monkeys[throwTo].Items }
            monkeys[i] <- { monkey with Items = tail }

            doThrow i (times + 1)

        | [] -> times

    let inspectedTimes = Array.create monkeys.Length 0
    
    for r = 1 to roundCount do
        for i = 0 to (monkeys.Length - 1) do 
            let times = doThrow i 0
            inspectedTimes[i] <- inspectedTimes[i] + times
        
    inspectedTimes
        
module Puzzle21 = 
    let solve input = 
        let monkeys = parseMonkeys input
        let relieve (x: int64) = x / 3L 
        let inspectedTimes = round 20 relieve monkeys 

        inspectedTimes
        |> Array.sortDescending
        |> Array.take 2
        |> Array.reduce (*) 

    let result = solve TestData.input

module Puzzle22 = 
    let solve input = 
        let monkeys = parseMonkeys input
        let safeMod = monkeys |> Array.map (fun m -> m.DivisibleBy) |> Array.reduce (*)
        let relieve x = x % safeMod
        let inspectedTimes = round 10000 relieve monkeys 

        inspectedTimes
        |> Array.sortDescending
        |> Array.take 2
        |> Array.map int64
        |> Array.reduce (*) 

    let result = solve TestData.input