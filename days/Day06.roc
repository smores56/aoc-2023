module [part1, part2]

import Utils exposing [graphemes]

parseNumbers = \chars ->
    (a, b) = List.walk chars ([], NoNum) \(nums, current), char ->
        when Str.toU64 char is
            Ok digit ->
                when current is
                    SomeNum n -> (nums, SomeNum (10 * n + digit))
                    NoNum -> (nums, SomeNum digit)

            Err _ ->
                when current is
                    SomeNum n -> (List.append nums n, NoNum)
                    NoNum -> (nums, NoNum)

    when b is
        SomeNum n -> List.append a n
        NoNum -> a

parseRaces = \lines ->
    getNums = \line ->
        line
        |> graphemes
        |> List.dropFirst 9
        |> parseNumbers

    times <- List.get lines 0
        |> Result.map getNums
        |> Result.try
    distances <- List.get lines 1
        |> Result.map getNums
        |> Result.map

    List.map2 times distances \time, distance ->
        { time, distance }

getBoundsOfRace = \{ time, distance } ->
    a = -1.0f64
    b = Num.toF64 time
    c = -1.0 * Num.toF64 distance
    (_first, _second) <- rootsOfQuadraticEquation a b c |> Result.map

    # TODO: commented out because of a compiler bug,
    #       for now un-comment and run with --linker=legacy
    # ((Num.floor first) + 1, (Num.ceiling second) - 1)

    (1, 1)

rootsOfQuadraticEquation = \a, b, c ->
    discriminant = (b * b) - (4 * a * c)
    sqrtDiscriminant <- Num.sqrtChecked discriminant |> Result.map

    ((-b + sqrtDiscriminant) / (2.0 * a), (-b - sqrtDiscriminant) / (2.0 * a))

part1 = \lines ->
    races = parseRaces lines |> Result.withDefault []
    raceBeatingBounds = List.keepOks races getBoundsOfRace

    List.walk raceBeatingBounds 1 \product, (left, right) ->
        product * (right - left + 1)
    |> Num.toStr

part2 = \lines ->
    getFullNumber = \line ->
        graphemes line
        |> List.dropFirst 9
        |> List.walk 0 \total, char ->
            when Str.toU64 char is
                Ok digit -> 10 * total + digit
                Err _ -> total

    time =
        List.get lines 0
        |> Result.withDefault ""
        |> getFullNumber
    distance =
        List.get lines 1
        |> Result.withDefault ""
        |> getFullNumber

    when getBoundsOfRace { time, distance } is
        Ok (left, right) -> Num.toStr (right - left + 1)
        Err _ -> "Error"
