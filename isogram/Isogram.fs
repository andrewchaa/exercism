module Isogram

let isIsogram (str: string) = 
    let rec count (c: char) (cs: char list) =
        match c with
        | ' ' | '-' -> 1
        | _ ->  match cs with
                | [] -> 0
                | x::xs ->  if (x = c) then 1 + count c xs
                            else count c xs
    
    str.ToLower() 
    |> Seq.toList
    |> fun xs -> List.forall (fun elem -> (count elem xs) = 1) xs

