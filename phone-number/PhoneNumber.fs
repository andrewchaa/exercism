module PhoneNumber

open System

let clean input: Result<uint64, string> = 

    let rec removeNoise (inputs: char list): char list =
        match inputs with
        | [] -> []
        | x::xs when x=' ' || x='-' || x='(' || x=')' || x='.' || x='+' -> removeNoise xs
        | x::xs -> x :: removeNoise xs
    
    let validateAgainstNonNumerics (inputs: char list): Result<char list, string> =
        if inputs |> List.exists Char.IsLetter then Error "alphanumerics not permitted"
        else if inputs |> List.exists (fun elem -> elem = '@' || elem = ':' || elem = '!') then Error "punctuations not permitted"
        else Ok inputs
        

    let validateDigits (inputs: Result<char list, string>) : Result<char list, string> =
        match inputs with
        | Ok numbers when numbers.Length = 9    -> Error "incorrect number of digits"
        | Ok numbers when numbers.Length > 11   -> Error "more than 11 digits"
        | _      -> inputs
        
    let removeCountryCode (inputs: Result<char list, string>): Result<char list, string> =
        match inputs with
        | Ok numbers when numbers.Length = 11 && numbers.[0] = '1' -> Ok (List.tail numbers)
        | Ok numbers when numbers.Length = 11 && numbers.[0] <> '1' -> Error "11 digits must start with 1"
        | _ -> inputs

    let validateCode (numbers: Result<char list, string>): Result<char list, string> =
        match numbers with
        | Ok ns when ns.[0] = '0'   -> Error "area code cannot start with zero"
        | Ok ns when ns.[0] = '1'   -> Error "area code cannot start with one"
        | Ok ns when ns.[3] = '0'   -> Error "exchange code cannot start with zero"
        | Ok ns when ns.[3] = '1'   -> Error "exchange code cannot start with one"
        | _      -> numbers

    let composeResult (result: Result<char list, string>) =
        match result with
        | Ok ns  -> ns |> String.Concat |> UInt64.Parse |> Ok
        | Error msg -> Error msg

    input 
    |> Seq.toList
    |> removeNoise 
    |> validateAgainstNonNumerics
    |> validateDigits
    |> removeCountryCode
    |> validateCode
    |> composeResult
    

    
