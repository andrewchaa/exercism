module PhoneNumber

open System

let clean input: Result<uint64, string> = 

    let rec removeNoise (inputs: char list): char list =
        match inputs with
        | [] -> []
        | x::xs ->  match x with
                    | ' ' | '-' | '(' | ')' | '.' -> removeNoise xs
                    | _ -> x :: removeNoise xs
    
    let removeCountryCode (inputs: char list): char list =
        if inputs.Length > 10 && inputs.[0] = '1' then List.tail inputs
        else inputs

    let numbers = input |> Seq.toList |> removeNoise |> removeCountryCode
    if (numbers.[0] = '0') then Error "area code cannot start with zero"
    else if (numbers.[0] = '1') then Error "area code cannot start with one"
    else numbers |> String.Concat |> UInt64.Parse |> Ok

    
