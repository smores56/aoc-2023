module [part1, part2]

import Utils exposing [graphemes]

parseSpringChar = \char ->
    when char is
        "?" -> Ok Unknown
        "#" -> Ok Broken
        "." -> Ok Functional
        _ -> Err (InvalidSpringChar char)

parseSpringLine = \line ->
    { before, after } <- Str.splitFirst line " "
        |> Result.try
    springs <- before
        |> graphemes
        |> List.mapTry parseSpringChar
        |> Result.try
    sizes <- after
        |> Str.split ","
        |> List.mapTry Str.toU64
        |> Result.map

    { springs, sizes }

# array is a 2d array
# first dimension is 0 to len springs, i being num arrangements from 0 to i
# second dimension is 0 to len sizes, i being num arrangements from 0 to i
#
# numArrs(spring, size) =
#     # base cases
#     if spring == 0 then
#         if size == 0 then
#             return 0 TODO maybe 1?
#         else
#             return 0
#     else if size == 0 then
#         TODO, either 0 or 1?
#
#     # recurrence relations
#     if last spring == "." then
#         return numArrs(spring - 1, size)
#     if last spring == "#" then
#         if last spring group fits in last size then
#             return numArrs(spring - size - 1, size - 1) + 1
#         else
#             return numArrs(spring - size - 1, size - 1) # or 0?
#     if last spring == "?" then
#         if last spring group fits in last size then
#             TODO

# . # . ? ? ? ? # ? . 1,1,3
#
# 0 0 0 0 0 0 0 0 0 0
#
# 0 1 1 1 1 1 1 0 0 0
#
# 0 0 0 1 2 3 4 1 1 1
#
# 0 0 0 0 0 0 0 1 3 3

numberOfBrokenSpringArrangements = \springs, sizes ->
    when List.first sizes is
        Err _ ->
            if List.any springs \spring -> spring == Broken then
                0
            else
                1

        Ok size if List.len springs < size -> 0
        Ok size ->
            firstBrokenIndex =
                List.findFirstIndex springs (\spring -> spring == Broken)
                |> Result.withDefault (List.len springs - 1)
            lastIndex = Num.min firstBrokenIndex (List.len springs - size)
            startingIndices = List.range { start: At 0, end: At lastIndex }
            possibleStartingIndices =
                startingIndices
                |> List.keepIf \start -> validBrokenSpringLocation springs start size
                |> List.keepIf \start -> List.get springs (start + size - 1)
                    |> Result.map \spring -> spring != Functional
                    |> Result.withDefault Bool.true

            possibleStartingIndices
            |> List.map \start ->
                remainingSprings = springs |> List.dropFirst (start + size + 1)
                remainingSizes = sizes |> List.dropFirst 1

                numberOfBrokenSpringArrangements remainingSprings remainingSizes
            |> List.sum

validBrokenSpringLocation = \springs, start, len ->
    allSpringsMightBeBroken =
        List.sublist springs { start, len }
        |> List.all \spring -> spring == Broken || spring == Unknown
    precedingSpringIsNotBroken =
        when start is
            0 -> Bool.true
            _ ->
                when List.get springs (start - 1) is
                    Ok s -> s != Broken
                    Err _ -> Bool.true
    followingSpringIsNotBroken =
        when List.get springs (start + len) is
            Ok spring -> spring != Broken
            Err _ -> Bool.true

    allSpringsMightBeBroken && precedingSpringIsNotBroken && followingSpringIsNotBroken

part1 = \lines ->
    springsAndSizes = List.keepOks lines parseSpringLine
    arrangementCounts = List.map springsAndSizes \{ springs, sizes } ->
        numberOfBrokenSpringArrangements springs sizes

    arrangementCounts
    |> List.sum
    |> Num.toStr

part2 = \lines ->
    springsAndSizes = List.keepOks lines parseSpringLine
    arrangementCounts = List.map springsAndSizes \{ springs, sizes } ->
        unfoldedSprings =
            List.repeat springs 5
            |> List.intersperse [Unknown]
            |> List.joinMap \x -> x
        unfoldedSizes =
            List.repeat sizes 5
            |> List.joinMap \x -> x

        numberOfBrokenSpringArrangements unfoldedSprings unfoldedSizes

    arrangementCounts
    |> List.sum
    |> Num.toStr
