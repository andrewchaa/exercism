module Hamming

open System

let distance (strand1: string) (strand2: string): int option = 
    let compareChar (c1: char) (c2: char): int = 
        if c1 = c2 then 0
        else 1
    
    let compare ((cs1: list<char>), (cs2: list<char>)): int option =
        match cs1, cs2 with
        | ([], []) -> Some(0)
        | (x::xs, y::ys) -> Some((compareChar x y) + (compare xs ys))
        | _ -> Some(0)

    if String.IsNullOrEmpty(strand1) && String.IsNullOrEmpty(strand2) then
        Some(0)
    else compare (Seq.toList(strand1), Seq.toList(strand2))