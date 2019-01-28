module TwelveDays

let recite (start: int) (stop: int) = 
    let words = [
        ("first", "a Partridge in a Pear Tree");
        ("second", "two Turtle Doves");
        ("third", "three French Hens");
        ("fourth", "four Calling Birds");
        ("fifth", "five Gold Rings");
        ("sixth", "six Geese-a-Laying");
        ("seventh", "seven Swans-a-Swimming");
        ("eighth", "eight Maids-a-Milking");
        ("ninth", "nine Ladies Dancing");
        ("tenth", "ten Lords-a-Leaping");
        ("eleventh", "eleven Pipers Piping");
        ("twelfth", "twelve Drummers Drumming");
    ]

    let getDay n = fst words.[n-1]
    let rec getPhrase n = 
        match n with
        | 1 -> snd words.[n-1]
        | 2 -> snd words.[n-1] + ", and " + getPhrase (n-1)
        | _ -> snd words.[n-1] + ", " + getPhrase (n-1)
    
    let rec getSentence st sp =
        let sentence = sprintf "On the %s day of Christmas my true love gave to me: %s." (getDay st) (getPhrase st)
        if (st = sp) then
            [ sentence ]
        else
            sentence :: getSentence (st + 1) sp

    getSentence start stop
