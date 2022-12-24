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

module Puzzle15 =
    let solve (grids: int[,]) = 
        let visible i j v = 
            let length1 = (grids |> Array2D.length1)
            let length2 = (grids |> Array2D.length2)

            match i, j, v with
            | _ when i = 0 || j = 0 || i = length1 - 1 || j = length2 - 1 -> true
            | _ ->
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
            let length1 = (grids |> Array2D.length1)
            let length2 = (grids |> Array2D.length2)

            match i, j, v with
            | _ when i = 0 || j = 0 || i = length1 - 1 || j = length2 - 1 -> 0
            | _ ->
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