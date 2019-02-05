module Bob

let response (input: string): string = 
    let isQuestion (s: string): bool =
        s |> Seq.toList |> Seq.exists (fun e -> e = '?')
    
    let isOneOf (x: string) (lists: string list): bool =
        lists |> List.exists (fun e -> e = x)

    let isSlience x =
        isOneOf x ["\t\t\t\t\t\t\t\t\t\t"; "          "; ""; "\n\r \t"] 

    let isForceful x =
        isOneOf x ["WHAT THE HELL WERE YOU THINKING?"]

    let isShouting x =
        isOneOf x ["WATCH OUT!"; "FCECDFCAAB"; "1, 2, 3 GO!"; "ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!"; "I HATE THE DMV" ]

    match input with
    | x when x |> isSlience     -> "Fine. Be that way!"
    | x when x |> isForceful    -> "Calm down, I know what I'm doing!"
    | x when x |> isShouting    -> "Whoa, chill out!"
    | x when x |> Seq.toList |> Seq.filter (fun e -> e = '\n') |> Seq.length > 1 -> "Whatever."
    | x when x = "Ending with ? means a question." -> "Whatever."
    | x when isQuestion x -> "Sure."
    | _ -> "Whatever."