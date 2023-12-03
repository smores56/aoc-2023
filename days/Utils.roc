interface Utils
    exposes [dropGraphemes, parseLiteral, parseNat]
    imports []

dropGraphemes = \s, amount ->
    s
    |> Str.graphemes
    |> List.dropFirst amount
    |> Str.joinWith ""

parseLiteral = \text, literal ->
    if Str.startsWith text literal then
        dropAmount = Str.countGraphemes literal
        Ok (dropGraphemes text dropAmount)
    else
        Err ParseError

parseNat = \text ->
    digits =
        text
        |> Str.graphemes
        |> List.walkUntil [] \digitsSoFar, char ->
            when Str.toNat char is
                Ok digit -> Continue (List.append digitsSoFar digit)
                Err _ -> Break digitsSoFar

    if (List.len digits) > 0 then
        total = List.walk digits 0 \sum, digit ->
            sum * 10 + digit

        Ok (total, dropGraphemes text (List.len digits))
    else
        Err ParseError
