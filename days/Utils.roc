module [
    graphemes,
    countGraphemes,
    dropGraphemes,
    parseLiteral,
    parseNat,
    lowerAndHigher,
]

graphemes = \s ->
    Str.toUtf8 s
    |> List.mapTry \c -> Str.fromUtf8 [c]
    |> Result.withDefault []

countGraphemes = \s ->
    Str.toUtf8 s
    |> List.len

dropGraphemes = \s, amount ->
    s
    |> graphemes
    |> List.dropFirst amount
    |> Str.joinWith ""

parseLiteral = \text, literal ->
    if Str.startsWith text literal then
        dropAmount = countGraphemes literal
        Ok (dropGraphemes text dropAmount)
    else
        Err ParseError

parseNat = \text ->
    digits =
        text
        |> graphemes
        |> List.walkUntil [] \digitsSoFar, char ->
            when Str.toU64 char is
                Ok digit -> Continue (List.append digitsSoFar digit)
                Err _ -> Break digitsSoFar

    if (List.len digits) > 0 then
        total = List.walk digits 0 \sum, digit ->
            sum * 10 + digit

        Ok (total, dropGraphemes text (List.len digits))
    else
        Err ParseError

lowerAndHigher = \a, b ->
    if a < b then (a, b) else (b, a)
