interface Day19
    exposes [part1, part2]
    imports [Utils, Range]

Category : [X, M, A, S]

parseCategory : Str -> Result Category [InvalidCategory]
parseCategory = \char ->
    when char is
        "x" -> Ok X
        "m" -> Ok M
        "a" -> Ok A
        "s" -> Ok S
        _ -> Err InvalidCategory

Destination : [Accept, Reject, GoToWorkflow Str]

parseDestination : Str -> Destination
parseDestination = \line ->
    chars = Str.graphemes line
    when List.first chars is
        Ok "A" -> Accept
        Ok "R" -> Reject
        _ -> GoToWorkflow line

Comparison : [LessThan, GreaterThan]

parseComparison : Str -> Result Comparison [InvalidComparison Str]
parseComparison = \char ->
    when char is
        "<" -> Ok LessThan
        ">" -> Ok GreaterThan
        _ -> Err (InvalidComparison char)

WorkflowStep : [
    Destination Destination,
    GoToIf
        {
            category : Category,
            comparison : Comparison,
            limit : Nat,
            destination : Destination,
        },
]

parseWorkflowStep : Str -> Result WorkflowStep [InvalidCategory, InvalidNumStr, ListWasEmpty, NotFound]
parseWorkflowStep = \line ->
    chars = Str.graphemes line
    comparisonResult =
        List.get chars 1
        |> Result.try parseComparison

    when comparisonResult is
        Err _ -> Ok (Destination (parseDestination line))
        Ok comparison ->
            category <- List.first chars
                |> Result.try parseCategory
                |> Result.try
            { before, after } <- line
                |> Utils.dropGraphemes 2
                |> Str.splitFirst ":"
                |> Result.try
            limit <- Str.toNat before
                |> Result.map
            destination = parseDestination after

            GoToIf { category, comparison, limit, destination }

parseWorkflow : Str -> Result (Str, List WorkflowStep) [NotFound, InvalidCategory, InvalidNumStr, ListWasEmpty]
parseWorkflow = \line ->
    { before: name, after: rest } <- Str.splitFirst line "{"
        |> Result.try
    { before: stepList, after: _ } <- Str.splitLast rest "}"
        |> Result.try

    steps <- stepList
        |> Str.split ","
        |> List.mapTry parseWorkflowStep
        |> Result.map

    (name, steps)

parseRating : Str -> Result (Dict Category Nat) [InvalidCategory, InvalidNumStr, NotFound]
parseRating = \line ->
    withoutBrackets =
        Str.graphemes line
        |> List.dropFirst 1
        |> List.dropLast 1
        |> Str.joinWith ""
    groups <- Str.split withoutBrackets ","
        |> List.mapTry \group ->
            { before, after } <- Str.splitFirst group "="
                |> Result.try
            category <- parseCategory before
                |> Result.try
            amount <- Str.toNat after
                |> Result.map

            (category, amount)
        |> Result.map

    Dict.fromList groups

parseInput : List Str -> Result { workflows : Dict Str (List WorkflowStep), ratings : List (Dict Category Nat) } [WorkflowIssue [NotFound, InvalidCategory, InvalidNumStr, ListWasEmpty], RatingIssue [InvalidCategory, InvalidNumStr, NotFound]]
parseInput = \lines ->
    emptyLineIndex =
        List.findFirstIndex lines Str.isEmpty
        |> Result.withDefault 0
    workflows <- lines
        |> List.takeFirst emptyLineIndex
        |> List.mapTry parseWorkflow
        |> Result.map Dict.fromList
        |> Result.mapErr WorkflowIssue
        |> Result.try
    ratings <- lines
        |> List.dropFirst (emptyLineIndex + 1)
        |> List.dropIf Str.isEmpty
        |> List.mapTry parseRating
        |> Result.mapErr RatingIssue
        |> Result.map

    { workflows, ratings }

rangesOfRatingsAccepted = \workflows ->
    allCategories = [X, M, A, S]
    startingRanges =
        allCategories
        |> List.map \category -> (category, { start: 1, end: 4000 })
        |> Dict.fromList

    rangesOfRatingsAcceptedInner workflows [(GoToWorkflow "in", startingRanges)] []

rangesOfRatingsAcceptedInner = \workflows, ratingsQueue, acceptedRanges ->
    when List.first ratingsQueue is
        Err _ -> acceptedRanges
        Ok (destination, ratingRanges) ->
            (queueAdditions, acceptedAdditions) =
                when destination is
                    Accept -> ([], [ratingRanges])
                    Reject -> ([], [])
                    GoToWorkflow workflowName ->
                        when Dict.get workflows workflowName is
                            Ok workflow -> (newRangeCandidates ratingRanges workflow, [])
                            Err _ -> ([], [])

            updatedQueue =
                ratingsQueue
                |> List.dropFirst 1
                |> List.concat queueAdditions
            updatedAcceptances =
                List.concat acceptedRanges acceptedAdditions

            rangesOfRatingsAcceptedInner workflows updatedQueue updatedAcceptances

# TODO: rename
newRangeCandidates = \ratingRanges, workflow ->
    (_leftoverRanges, reworkedCandidates) =
        List.walk workflow (ratingRanges, []) \(remainingRanges, candidates), workflowStep ->
            when workflowStep is
                GoToIf context ->
                    (remainingRangesAfterSplit, newCandidates) = splitRangesAtGoTo remainingRanges context
                    (remainingRangesAfterSplit, List.concat candidates newCandidates)

                Destination destination ->
                    (Dict.empty {}, List.append candidates (destination, remainingRanges))

    reworkedCandidates

splitRangesAtGoTo = \ratingRanges, context ->
    splitPointResult =
        ratingRanges
        |> Dict.toList
        |> List.findFirst \(category, range) ->
            Range.contains range context.limit && context.category == category

    when splitPointResult is
        Err _ -> (ratingRanges, [])
        Ok (categoryToSplit, rangeToSplit) ->
            (remainingRange, splitOffRange) =
                when context.comparison is
                    LessThan ->
                        (
                            { start: context.limit, end: rangeToSplit.end },
                            { start: rangeToSplit.start, end: context.limit - 1 },
                        )

                    GreaterThan ->
                        (
                            { start: rangeToSplit.start, end: context.limit },
                            { start: context.limit + 1, end: rangeToSplit.end },
                        )

            (
                Dict.insert ratingRanges categoryToSplit remainingRange,
                [(context.destination, Dict.insert ratingRanges categoryToSplit splitOffRange)],
            )

part1 = \lines ->
    { workflows, ratings } =
        when parseInput lines is
            Ok data -> data
            Err err -> crash "Assumed input was well-formed: \(Inspect.toStr err)"

    acceptedRanges = rangesOfRatingsAccepted workflows
    acceptedRatings = List.keepIf ratings \rating ->
        acceptedRanges
        |> List.any \ranges ->
            ranges
            |> Dict.toList
            |> List.all \(category, range) ->
                when Dict.get rating category is
                    Ok value -> Range.contains range value
                    Err _ -> Bool.false

    acceptedRatings
    |> List.map \rating ->
        Dict.values rating |> List.sum
    |> List.sum
    |> Num.toStr

part2 = \lines ->
    { workflows, ratings: _ } =
        when parseInput lines is
            Ok data -> data
            Err err -> crash "Assumed input was well-formed: \(Inspect.toStr err)"

    acceptedRanges = rangesOfRatingsAccepted workflows

    acceptedRanges
    |> List.map \ratingRanges ->
        ratingRanges
        |> Dict.toList
        |> List.map \(_category, range) -> Range.len range
        |> List.product
    |> List.sum
    |> Num.toStr
