module Day9

open System.IO

module private TestData = 
    let input = File.ReadAllLines("Day9/input.txt")

type Cmd = string * int

let parseCmds (lines: string array) = 
    lines
    |> Array.map (fun l -> l.Split(' '))
    |> Array.map (fun a -> a[0], int a[1])

let isAdjacent (x1, y1) (x2, y2) = 
    abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

let follow (tx, ty) (hx, hy) =
    let distance x y = abs (x - y)

    match distance hx tx, distance hy ty with
    | 0, 2 when hy = ty + 2 -> (tx, ty + 1)
    | 0, 2 when hy = ty - 2 -> (tx, ty - 1)
    | 2, 0 when hx = tx + 2 -> (tx + 1, ty)
    | 2, 0 when hx = tx - 2 -> (tx - 1, ty)
    | 1, 1 -> (tx, ty)
    | n, m when n >= 1 && m >= 1 ->
        let ntx = if hx > tx then tx + 1 else tx - 1
        let nty = if hy > ty then ty + 1 else ty - 1
        (ntx, nty)
    | _ -> (tx, ty)

let rec move (knots, visited) cmd = 
    if snd cmd = 0 then
        knots, visited
    else
        match knots with
        | head :: rest ->
            let hx, hy = head
            let head' = 
                match cmd with
                | "R", _ -> hx + 1, hy
                | "L", _ -> hx - 1, hy
                | "U", _ -> hx, hy + 1
                | "D", _ -> hx, hy - 1
                | _ -> failwith "Invalid cmd"

            let knots' = rest |> List.fold (fun ks k -> ks @ [(follow k (ks |> List.last))]) [head']
            let visited' = Set.add (knots' |> List.last) visited

            move (knots', visited') (fst cmd, snd cmd - 1)

        | _ -> failwith "Invalid knots"

module Puzzle17 = 
    let solve lines = 
        let cmds = parseCmds lines
        let visited = [(0, 0)] |> Set.ofList
        let initState = [(0, 0); (0, 0)], visited

        let _, finalVisited = 
            cmds
            |> Array.fold (fun state cmd -> move state cmd) initState

        finalVisited |> Set.count

    let result = solve TestData.input

module Puzzle18 = 
    let solve lines = 
        let cmds = parseCmds lines
        let visited = [(0, 0)] |> Set.ofList
        let knots = (Array.init 10 (fun _ -> (0, 0))) |> List.ofArray
        let initState = knots, visited

        let _, finalVisited = 
            cmds
            |> Array.fold (fun state cmd -> move state cmd) initState

        finalVisited |> Set.count

    let result = solve TestData.input