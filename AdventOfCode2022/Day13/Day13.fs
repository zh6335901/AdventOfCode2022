module Day13

open System.IO
open System

module private TestData = 
    let input = 
        File.ReadAllLines("Day13/input.txt") 
        |> Array.filter (fun s -> not (String.IsNullOrEmpty(s)))

type Packet = 
   | Single of int
   | List of Packet list

let rec splitAt (positions: int list) (str: string) = 
    let rec splitAt' (str: string) cur positions = 
        match positions with
        | head :: tail -> str.Substring(cur, head - cur) :: (splitAt' str (head + 1) tail)
        | [] -> [str.Substring(cur)]

    splitAt' str 0 positions

let parsePackets (lines: string array) = 
    let rec findTopCommaPositions (txt: string) level i ps = 
        if i = txt.Length then
            ps
        else
            match txt[i] with
            | '[' -> findTopCommaPositions txt (level + 1) (i + 1) ps
            | ']' -> findTopCommaPositions txt (level - 1) (i + 1) ps
            | ',' when level = 0 -> findTopCommaPositions txt level (i + 1) (ps @ [i])
            | _ -> findTopCommaPositions txt level (i + 1) ps

    let rec parsePacket (txt: string) = 
        if txt.StartsWith('[') then
            let nt = txt.Substring(1, txt.Length - 2)
            
            if nt = String.Empty then
                List []
            else
                let ps = findTopCommaPositions nt 0 0 []
                let packetTxts = splitAt ps nt

                List (packetTxts |> List.map parsePacket)
        else
            Single (Int32.Parse txt)

    lines |> Array.map parsePacket

let rec inRightOrder l r = 
    match l, r with
    | Single lv, Single rv -> if lv = rv then None else Some (lv < rv)
    | Single lv, List rv -> inRightOrder (List [Single lv]) (List rv)
    | List lv, Single rv -> inRightOrder (List lv) (List [Single rv])
    | List lv, List rv -> 
        match lv, rv with
        | lh :: lt, rh :: rt -> 
            match inRightOrder lh rh with
            | None -> inRightOrder (List lt) (List rt)
            | x -> x
        | _ :: _, [] -> Some false
        | [], _ :: _ -> Some true
        | [], [] -> None

module Puzzle25 =
    let solve lines = 
        lines
        |> parsePackets
        |> Array.chunkBySize 2
        |> Array.indexed
        |> Array.map (fun (i, p) -> i, inRightOrder p[0] p[1])
        |> Array.filter (fun (_, o) -> match o with | Some x -> x | None -> false)
        |> Array.sumBy (fun (i, _) -> i + 1)

    let result = solve TestData.input

module Puzzle26 = 
    let solve lines = 
        let rec findIndex divider packets index = 
            match packets with
            | [] -> index
            | head :: tail -> 
                let v = 
                    match inRightOrder divider head with
                    | Some true -> 0
                    | _ -> 1

                findIndex divider tail (index + v)

        let packets  = lines |> parsePackets |> Array.toList
        let divider2 = List ([List ([Single 2])])
        let divider6 = List ([List ([Single 6])])

        let indexOfDivider2 = findIndex divider2 packets 1
        let indexOfDivider6 = findIndex divider6 packets 2

        indexOfDivider2 * indexOfDivider6

    let result = solve TestData.input