module Pangram

let isPangram (input: string): bool = 
    "abcdefghijklmnopqrstuvwxyz" 
    |> Seq.toList
    |> List.forall (fun elem -> input.ToLower().IndexOf(elem) > -1)
