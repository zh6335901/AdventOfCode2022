module Day7

open System.IO

module private TestData = 
    let input = File.ReadAllLines("Day7/input.txt")

type FileTree = 
    | File of string * int
    | Dir of string * FileTree list

type Output = 
    | CD of string
    | LS
    | FileInfo of string * int
    | DirInfo of string

let parseOutputs (lines: string array) = 
    lines
    |> List.ofArray
    |> List.map (
        function
        | x when x.StartsWith "$ cd" -> x.Split(" ") |> Array.last |> CD
        | x when x.StartsWith "$ ls" -> LS
        | x when x.StartsWith "dir" -> x.Split(" ") |> Array.last |> DirInfo
        | x -> let a = x.Split(" ") in (a[1], int a[0]) |> FileInfo
    )

let build lines = 
    let rec buildFileTree cur outputs = 
        let (@@) n x = n + x + "/"

        match outputs with
        | output :: tail ->
            match cur with
            | Dir (n, trees) ->               
                match output with
                | CD x ->
                    if x = ".." then
                        cur, tail
                    else
                        let dir = Dir (n @@ x, [])
                        let child, outputs' = buildFileTree dir tail
                        let cur' = Dir (n, trees @ [child])
                        buildFileTree cur' outputs'

                | FileInfo (x, s) -> 
                    let child = File (n @@ x, s)
                    let cur' = Dir (n, trees @ [child])
                    buildFileTree cur' tail

                | _ -> buildFileTree cur tail

            | File (n, s) -> File(n, s), tail

        | _ -> cur, []
    
    let outputs = parseOutputs lines
    let root = Dir ("/", [])
    let tree, _ = buildFileTree root (outputs |> List.tail)

    tree

module Puzzle13 =
    let solve lines = 
        let tree = build lines
        let mutable smalls: int list = []

        let rec stat tree =
            match tree with
            | Dir (_, trees) ->
                let size = trees |> List.map stat |> List.sum
                if size <= 10_0000 then
                    smalls <- size :: smalls
                    size
                else 
                    size
            | File (_, size) -> size

        stat tree |> ignore

        smalls |> List.sum

    let result = solve TestData.input

module Puzzle14 = 
    let solve lines = 
        let tree = build lines
        let mutable dirSizes = []

        let rec collect tree = 
            match tree with
            | Dir (_, trees) ->
                let size = trees |> List.map collect |> List.sum
                dirSizes <- size :: dirSizes
                size

            | File (_, size) -> size

        collect tree |> ignore

        let total = dirSizes |> List.max

        dirSizes
        |> List.sort
        |> List.find (fun x -> 7000_0000 - total + x >= 3000_0000)

    let result = solve TestData.input