module [part1, part2]

import Utils exposing [graphemes]

parseCard = \line ->
    { before: _, after } <- Str.splitFirst line ":" |> Result.try
    { before: cardNumbersStr, after: myNumbersStr } <- Str.splitFirst after " | " |> Result.map

    { cardNumbers: parseNumbers cardNumbersStr, myNumbers: parseNumbers myNumbersStr }

parseNumbers : Str -> List U64
parseNumbers = \text ->
    trimmed = Str.trim text
    (allNums, lastNum) =
        graphemes trimmed
        |> List.walk ([], NoNum) \(nums, num), char ->
            when Str.toU64 char is
                Ok digit ->
                    when num is
                        SomeNum n -> (nums, SomeNum (10 * n + digit))
                        NoNum -> (nums, SomeNum digit)

                Err _ ->
                    when num is
                        SomeNum n -> (List.append nums n, NoNum)
                        NoNum -> (nums, NoNum)

    when lastNum is
        SomeNum n -> List.append allNums n
        NoNum -> allNums

part1 = \lines ->
    cards = List.keepOks lines parseCard
    cardScores = List.map cards \card ->
        amountFound = List.countIf card.cardNumbers \cn ->
            card.myNumbers |> List.contains cn

        if amountFound == 0 then
            0
        else
            Num.powInt 2 (amountFound - 1)

    cardScores
    |> List.sum
    |> Num.toStr

part2 = \lines ->
    cards = List.keepOks lines parseCard

    startingCardCounts =
        List.range { start: At 1, end: At (List.len cards) }
        |> List.map \card -> (card, 1)
        |> Dict.fromList
    cardCounts = List.walkWithIndex cards startingCardCounts \counts, card, index ->
        cardNum = index + 1
        amountFound = List.countIf card.cardNumbers \cn ->
            List.contains card.myNumbers cn

        lastCardWon = Num.min (cardNum + amountFound) (List.len cards)
        cardsWon = List.range { start: After cardNum, end: At lastCardWon }
        currentCount = Dict.get counts cardNum |> Result.withDefault 0

        List.walk cardsWon counts \newCounts, cardWon ->
            currentWonAmount = Dict.get newCounts cardWon |> Result.withDefault 0
            newCounts |> Dict.insert cardWon (currentWonAmount + currentCount)

    Dict.values cardCounts
    |> List.sum
    |> Num.toStr
