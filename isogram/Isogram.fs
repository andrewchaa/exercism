module Isogram

let isIsogram (str: string) = 
    let rec count ((c: char), (cs: char list)) =
        match c, cs with
        | ' ', _    -> 1
        | '-', _    -> 1
        | _, []     -> 0
        | x, y::ys  -> if (x = y) then 1 + count (x, ys)
                       else count (x, ys)
    
    str.ToLower() 
    |> Seq.toList
    |> fun xs -> List.forall (fun elem -> (count (elem, xs)) = 1) xs

