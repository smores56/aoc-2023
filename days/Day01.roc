module [part1, part2]

import Utils exposing [graphemes]

digitNames = [
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
]

firstAndLastNumberOnLine = \line, findDigits ->
    chars = graphemes line
    digits = findDigits chars

    firstDigit = List.first digits |> Result.withDefault 0
    lastDigit = List.last digits |> Result.withDefault 0

    firstDigit * 10 + lastDigit

digitsWordOrNumber = \chars ->
    startingIndices = List.range { start: At 0, end: Before (List.len chars) }
    List.keepOks startingIndices \index ->
        firstDigit =
            List.get chars index
            |> Result.try Str.toU64

        when firstDigit is
            Ok digit -> Ok digit
            Err _ ->
                rest = List.dropFirst chars index |> Str.joinWith ""
                List.findFirstIndex digitNames \name -> Str.startsWith rest name

part1 = \lines ->
    numbers = List.map lines \line ->
        findDigits = \chars -> List.keepOks chars Str.toU64
        firstAndLastNumberOnLine line findDigits

    Num.toStr (List.sum numbers)

part2 = \lines ->
    numbers = List.map lines \line ->
        firstAndLastNumberOnLine line digitsWordOrNumber

    Num.toStr (List.sum numbers)
