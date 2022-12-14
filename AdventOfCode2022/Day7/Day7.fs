module Day7

open System.IO

module private TestData = 
    let input = File.ReadAllLines("Day7/input.txt")

type File = { Name: string; Size: int }
type Directory = { Name: string; Files: File list; Directories: Directory list; TotalSize: int }
type CommandLine = 
    | CD of string
    | LS
    | FileInfo of string * int
    | DirInfo of string

let parseCommandLines (lines: string array) = 
    lines
    |> List.ofArray
    |> List.map (
        function
        | x when x.StartsWith "$ cd" -> x.Split(" ") |> Array.last |> CD
        | x when x.StartsWith "$ ls" -> LS
        | x when x.StartsWith "dir" -> x.Split(" ") |> Array.last |> DirInfo
        | x -> let a = x.Split(" ") in (a[1], int a[0]) |> FileInfo
    )

let rec buildDir dir commandLines =
    let rec build dir commandLines = 
        match commandLines with
        | cl :: rest ->
            match cl with
            | CD ".." -> dir, rest

            | CD p ->
                let childDir = { Name = p; Files = []; Directories = []; TotalSize = 0 }
                let childDir', rest' = build childDir rest
                let dir' = { dir with Directories = childDir' :: dir.Directories; TotalSize = dir.TotalSize + childDir'.TotalSize }
                build dir' rest'

            | FileInfo (name, size) -> 
                let dir' = { dir with Files = { Name = name; Size = size } :: dir.Files; TotalSize = dir.TotalSize + size }
                build dir' rest

            | _ -> build dir rest

        | _ -> dir, []

    (build dir commandLines) |> fst

module Puzzle13 =
    let solve lines = 
        let commandLines = (parseCommandLines lines) |> List.tail
        let emptyRootDir = { Name = "/"; Files = []; Directories = []; TotalSize = 0 }
        let rootDir = buildDir emptyRootDir commandLines

        let rec stats (dir: Directory) = 
            let childDirs = (dir.Directories |> List.map stats |> List.concat)

            if dir.TotalSize < 10_0000 then dir :: childDirs
            else childDirs

        stats rootDir
        |> List.sumBy (fun d -> d.TotalSize)
                
    let result = solve TestData.input

module Puzzle14 = 
    let solve lines = 
        let commandLines = (parseCommandLines lines) |> List.tail
        let emptyRootDir = { Name = "/"; Files = []; Directories = []; TotalSize = 0 }
        let rootDir = buildDir emptyRootDir commandLines

        let rec collect dir = 
            let childDirs = (dir.Directories |> List.map collect |> List.concat)
            dir :: childDirs

        let dirSizes = rootDir |> collect |> List.map (fun d -> d.TotalSize)
        let total = dirSizes |> List.max

        dirSizes
        |> List.sort
        |> List.find (fun x -> 7000_0000 - total + x >= 3000_0000)

    let result = solve TestData.input