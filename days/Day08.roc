interface Day08
    exposes [part1, part2]
    imports []

parseInstructions = \lines ->
    turns =
        List.get lines 0
        |> Result.map parseTurns
        |> Result.withDefault []

    forks =
        List.dropFirst lines 2
        |> List.map parseFork
        |> Dict.fromList

    (turns, forks)

parseTurns = \line ->
    Str.graphemes line
    |> List.map \char ->
        if char == "L" then Left else Right

parseFork = \line ->
    chars = Str.graphemes line
    substring = \from, len ->
        List.sublist chars { start: from, len }
        |> Str.joinWith ""

    start = substring 0 3
    left = substring 7 3
    right = substring 12 3

    (start, { left, right })

findStepsUntilGoal = \from, soFar, context ->
    if context.atGoal from then
        soFar
    else
        next = findNextStep from soFar context
        findStepsUntilGoal next (soFar + 1) context

findNextStep = \start, soFar, context ->
    turnIndex = soFar % (List.len context.turns)
    turn =
        context.turns
        |> List.get turnIndex
        |> Result.withDefault Left

    when Dict.get context.forks start is
        Err _ -> crash "All forks assumed to be provided"
        Ok fork ->
            if turn == Left then fork.left else fork.right

leastCommonMultiple = \nums ->
    allFactors = List.walk nums (Dict.empty {}) \lcmFactors, num ->
        numFactors = primeFactors num
        remainingFactors = removeCounts numFactors lcmFactors

        combineCounts lcmFactors remainingFactors

    Dict.walk allFactors 1 \product, factor, amount ->
        product * (Num.powInt factor amount)

primeFactors = \n ->
    if n == 1 then
        Dict.empty {}
    else
        nextFactor =
            List.range { start: At 2, end: At (n // 2) }
            |> List.findFirst \factor -> (n % factor) == 0

        when nextFactor is
            Err _ -> Dict.single n 1
            Ok f ->
                primeFactors (n // f)
                |> combineCounts (Dict.single f 1)

removeCounts = \base, toRemove ->
    Dict.walk toRemove base \counts, num, count ->
        value = Dict.get counts num |> Result.withDefault 0
        Dict.insert counts num (Num.subSaturated value count)

combineCounts = \base, additional ->
    Dict.walk additional base \counts, num, count ->
        value = Dict.get counts num |> Result.withDefault 0
        Dict.insert counts num (value + count)

part1 = \lines ->
    (turns, forks) = parseInstructions lines
    context = { turns, forks, atGoal: \pos -> pos == "ZZZ" }
    stepCount = findStepsUntilGoal "AAA" 0 context

    Num.toStr stepCount

part2 = \lines ->
    (turns, forks) = parseInstructions lines
    context = { turns, forks, atGoal: \pos -> Str.endsWith pos "Z" }

    stepCounts =
        Dict.keys forks
        |> List.keepIf \start -> Str.endsWith start "A"
        |> List.map \start -> findStepsUntilGoal start 0 context

    leastCommonMultiple stepCounts |> Num.toStr
