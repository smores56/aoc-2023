interface Day2
    exposes [part1, part2]
    imports [Utils.{ parseLiteral, parseNat }]

emptyHand = { red: 0, green: 0, blue: 0 }

parseGame = \line ->
    afterGame <- parseLiteral line "Game " |> Result.try
    (id, afterId) <- parseNat afterGame |> Result.try
    afterColon <- parseLiteral afterId ": " |> Result.try

    roundTexts = Str.split afterColon "; "
    rounds <- List.mapTry roundTexts parseRound |> Result.map

    { id, rounds }

parseRound = \text ->
    items = Str.split text ", "

    List.walkTry items emptyHand \counts, item ->
        (amount, rest) <- parseNat item |> Result.try
        afterSpace <- parseLiteral rest " " |> Result.map
        when afterSpace is
            "red" -> { counts & red: counts.red + amount }
            "green" -> { counts & green: counts.green + amount }
            "blue" -> { counts & blue: counts.blue + amount }
            _ -> counts

part1 = \input ->
    games = List.keepOks input parseGame
    roundIsLegal = \round ->
        round.red <= 12 && round.green <= 13 && round.blue <= 14

    legalIds =
        games
        |> List.keepOks \game ->
            if List.all game.rounds roundIsLegal then
                Ok game.id
            else
                Err IllegalRound

    List.sum legalIds |> Num.toStr

part2 = \input ->
    games = List.keepOks input parseGame
    minNeededCubes = \game ->
        List.walk game.rounds emptyHand \set, round -> {
            red: Num.max set.red round.red,
            green: Num.max set.green round.green,
            blue: Num.max set.blue round.blue,
        }
    setPower = \set ->
        set.red * set.green * set.blue

    games
    |> List.map minNeededCubes
    |> List.map setPower
    |> List.sum
    |> Num.toStr
