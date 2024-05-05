module [part1, part2]

import Utils exposing [graphemes]

parseCard = \char, jType ->
    when Str.toU64 char is
        Ok digit -> Ok digit
        Err _ ->
            when char is
                "A" -> Ok 14
                "K" -> Ok 13
                "Q" -> Ok 12
                "J" ->
                    when jType is
                        Jack -> Ok 11
                        Joker -> Ok 1

                "T" -> Ok 10
                _ -> Err InvalidCard

parseHand = \line, jType ->
    { before, after } <- line
        |> Str.splitFirst " "
        |> Result.try

    cards <- before
        |> graphemes
        |> List.mapTry \char -> parseCard char jType
        |> Result.try

    bid <- after
        |> Str.trim
        |> Str.toU64
        |> Result.map

    { cards, bid }

compareHands = \left, right, jType ->
    leftTypeScore = handTypeScore left.cards jType
    rightTypeScore = handTypeScore right.cards jType
    typeScoreCmp = Num.compare leftTypeScore rightTypeScore

    if typeScoreCmp != EQ then
        typeScoreCmp
    else
        List.map2 left.cards right.cards Pair
        |> List.keepOks \Pair l r ->
            cmp = Num.compare l r
            if cmp != EQ then Ok cmp else Err {}
        |> List.first
        |> Result.withDefault EQ

handTypeScore = \cards, jType ->
    { maxCount, pairCount } = getRelevantCardTypeCounts cards jType

    when maxCount is
        5 -> 7
        4 -> 6
        3 -> if pairCount == 1 then 5 else 4
        2 -> if pairCount == 2 then 3 else 2
        1 -> 1
        _ -> 0

getRelevantCardTypeCounts = \cards, jType ->
    typeCounts = groupCardTypes cards
    countsSansJoker = Dict.remove typeCounts 1 |> Dict.values

    pairCount = List.countIf countsSansJoker \c -> c == 2
    jokerCount = Dict.get typeCounts 1 |> Result.withDefault 0
    maxCountSansJokers = List.max countsSansJoker |> Result.withDefault 0

    when jType is
        Joker if jokerCount > 0 ->
            maxWithJokerCount = maxCountSansJokers + jokerCount
            newPairCount =
                when maxWithJokerCount is
                    3 -> if pairCount == 2 then 1 else 0
                    2 -> 1
                    _ -> 0

            { maxCount: maxWithJokerCount, pairCount: newPairCount }

        Jack | Joker ->
            { maxCount: maxCountSansJokers, pairCount }

groupCardTypes = \cards ->
    List.walk cards (Dict.empty {}) \counts, card ->
        currentCount = Dict.get counts card |> Result.withDefault 0
        Dict.insert counts card (currentCount + 1)

part1 = \lines ->
    totalWinningsForInput lines Jack

part2 = \lines ->
    totalWinningsForInput lines Joker

totalWinningsForInput = \lines, jType ->
    allHands = List.keepOks lines \line -> parseHand line jType
    sortedHands = List.sortWith allHands \l, r -> compareHands l r jType

    sortedHands
    |> List.mapWithIndex \hand, index -> Num.toI128 (hand.bid * (index + 1))
    |> List.sum
    |> Num.toStr
