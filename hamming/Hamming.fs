module Hamming

open System

let distance (strand1: string) (strand2: string): int option = 
    let compareChar (c1: char) (c2: char): int = 
        if c1 = c2 then 0
        else 1
    
    let rec compare ((cs1: char list), (cs2: char list)): int =
        match cs1, cs2 with
        | (x::xs, y::ys) -> (compareChar x y) + (compare (xs, ys))
        | _, _ -> 0

    if String.IsNullOrEmpty strand1 && String.IsNullOrEmpty strand2 then Some(0)
    else if strand1.Length <> strand2.Length then None
    else Some(compare (Seq.toList(strand1), Seq.toList(strand2)))

