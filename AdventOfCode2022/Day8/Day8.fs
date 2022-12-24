module Day8

open System.IO

module private TestData = 
    let input = File.ReadAllLines("Day8/input.txt")
    let toDigit c = int c - int '0'
    let grids = Array2D.init input.Length input[0].Length (fun i j -> input[i][j] |> toDigit)

let neighbours (grids: int[,]) r c =
    let left, right = 
        grids[r, *] 
        |> Array.splitAt c 
        |> (fun (l, r) -> (l |> Array.rev, r |> Array.tail))

    let above, below = 
        grids[*, c] 
        |> Array.splitAt r 
        |> (fun (a, b) -> (a |> Array.rev, b |> Array.tail))

    [ left; right; above; below ]

let isEdge (grids: int[,]) r c =
    let length1 = (grids |> Array2D.length1)
    let length2 = (grids |> Array2D.length2)

    r = 0 || c = 0 || r = length1 - 1 || c = length2 - 1

module Puzzle15 =
    let solve (grids: int[,]) = 
        let visible i j v = 
            match isEdge grids i j with
            | true -> true
            | false ->
                neighbours grids i j
                |> Seq.exists (fun ns -> ns |> Seq.forall (fun n -> n < v))

        grids
        |> Array2D.mapi (fun i j v -> visible i j v)
        |> Seq.cast<bool>
        |> Seq.filter id
        |> Seq.length

    let result = solve TestData.grids

module Puzzle16 = 
    let solve (grids: int[,]) =  
        let score i j v =
            match isEdge grids i j with
            | true -> 0
            | false ->
                let rec grab trees = 
                    match trees with
                    | [] -> []
                    | t :: ts when t < v -> t :: (grab ts)
                    | t :: _ -> [t]
                
                neighbours grids i j
                |> Seq.map (fun ns -> ns |> List.ofArray |> grab)
                |> Seq.map (fun ns -> (ns |> Seq.length))
                |> Seq.reduce (*)

        grids
        |> Array2D.mapi (fun i j v -> score i j v)
        |> Seq.cast<int>
        |> Seq.max

    let result = solve TestData.grids