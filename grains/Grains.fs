module Grains

open System

let square (n: int): Result<uint64,string> = 
    if n < 1 || n > 64 then Error "square must be between 1 and 64"
    else 
        let rec go nu: uint64 =
            match nu with
            | 1 -> 1UL
            | _ -> 2UL * go(nu - 1)
    
        n |> go |> uint64 |> Ok

let total: Result<uint64,string> = 
    let getValue (r: Result<uint64, string>): uint64 =
        match r with
        | Ok x -> x
        | _ -> 0UL

    [1..64] |> List.sumBy (fun el -> (square el) |> getValue ) |> Ok
