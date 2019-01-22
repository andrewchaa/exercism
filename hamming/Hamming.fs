module Hamming

open System

let distance (strand1: string) (strand2: string): int option = 
    let compareChar c1 c2 = 
        if c1 = c2 then 0
        else 1
    
    let rec compare ((cs1: char list), (cs2: char list)) =
        match cs1, cs2 with
        | (x::xs, y::ys) -> (compareChar x y) + (compare (xs, ys))
        | ([], []) -> 0
        | _, _ -> 0


    match strand1, strand2 with
    | (null, _) -> None
    | (_, null) -> None
    | (x, y) when x.Length <> y.Length -> None
    | (x, y) -> Some(compare (Seq.toList(x), Seq.toList(y)))
