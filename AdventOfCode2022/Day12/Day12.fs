module Day12

open System
open System.IO
open System.Collections.Generic

module private TestData =
    let input = File.ReadAllLines("Day12/input.txt")

let parseMap (lines: string array) = 
    lines |> Array.map (fun l -> l.ToCharArray())

type Point = int * int

let findPoints (map: char array array) ch = 
    map
    |> Array.mapi (fun i row -> row |> Array.mapi (fun j e -> (i, j, e)))
    |> Array.collect id
    |> Array.filter (fun (_, _, e) -> e = ch)
    |> Array.map (fun (i, j, _) -> (i, j))

let fewest (map: char array array) sp = 
    let createPriorityQueue () = PriorityQueue<Point, int>()
    let dequeue (pq: PriorityQueue<Point, int>) = if pq.Count > 0 then Some (pq.Dequeue()) else None
    let enqueue (pq: PriorityQueue<Point, int>) point priority = pq.Enqueue(point, priority)

    let row = map.Length
    let col = map[0].Length

    let adjacent (x, y) = 
        let valueOf i j = 
            match map[i][j] with
            | 'S' -> int 'a'
            | 'E' -> int 'z'
            | x   -> int x

        [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)]
        |> List.filter (
            fun (nx, ny) -> 
                nx >= 0 
                && nx < row 
                && ny >= 0 
                && ny < col 
                && int (valueOf x y) + 1 >= int (valueOf nx ny)
        )

    let ep = 'E' |> findPoints map |> Array.head

    let initDist x y = if (x, y) = sp then 0 else Int32.MaxValue
    let distTo = Array.init row (fun r -> Array.init col (fun c -> initDist r c))

    let queue = createPriorityQueue ()
    enqueue queue sp 0

    let mutable over = false

    while not over do
        let p = dequeue queue
        match p with
        | None -> over <- true
        | Some p when p = ep -> over <- true
        | Some (x, y) -> 
            let tos = adjacent (x, y)
            for tx, ty in tos do
                let d = distTo[x][y] + 1
                if d < distTo[tx][ty] then
                    distTo[tx][ty] <- d
                    enqueue queue (tx, ty) d
           
    let ex, ey = ep in distTo[ex][ey]

module Puzzle23 = 
    let solve lines = 
        let map = parseMap lines
        let sp = findPoints map 'S' |> Array.head
        fewest map sp

    let result = solve TestData.input

module Puzzle24 = 
    let solve lines = 
        let map = parseMap lines
        let sp = findPoints map 'S'
        let ap = findPoints map 'a'

        [sp; ap]
        |> Array.concat
        |> Array.map (fun p -> fewest map p)
        |> Array.min

    let result = solve TestData.input