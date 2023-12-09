interface Day09
    exposes [part1, part2]
    imports []

parseHistory = \line ->
    line
    |> Str.split " "
    |> List.mapTry Str.toI64

predictPoint = \history, timeToPredict ->
    if List.all history \d -> d == 0 then
        Ok 0
    else
        deltas = historyDeltas history
        nextDelta <- predictPoint deltas timeToPredict
            |> Result.try

        when timeToPredict is
            Before ->
                List.first history
                |> Result.map \first -> first - nextDelta

            After ->
                List.last history
                |> Result.map \last -> last + nextDelta

historyDeltas = \history ->
    shiftedHistory = List.dropFirst history 1
    List.map2 history shiftedHistory \a, b -> b - a

part1 = \lines ->
    histories = List.keepOks lines parseHistory
    nextPoints = List.keepOks histories \point ->
        predictPoint point After

    List.sum nextPoints |> Num.toStr

part2 = \lines ->
    histories = List.keepOks lines parseHistory
    nextPoints = List.keepOks histories \point ->
        predictPoint point Before

    List.sum nextPoints |> Num.toStr
