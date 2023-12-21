interface Day21
    exposes [part1, part2]
    imports [Grid]

parseGardens = \lines ->
    lines
    |> List.dropIf Str.isEmpty
    |> List.walkWithIndex ({ row: 0, column: 0 }, []) \(startingCoords, rows), line, rowIndex ->
        (foundCoords, row) =
            line
            |> Str.graphemes
            |> List.walkWithIndex (startingCoords, []) \(coords, plots), char, columnIndex ->
                when char is
                    "S" -> ({ row: rowIndex, column: columnIndex }, List.append plots Garden)
                    "." -> (coords, List.append plots Garden)
                    _ -> (coords, List.append plots Rock)

        (foundCoords, List.append rows row)

findReachablePlots = \gardens, startingCoords, steps ->
    innerFindReachablePlots gardens [] [(startingCoords, steps)]

innerFindReachablePlots = \gardens, visited, locationsToVisit ->
    if List.len locationsToVisit == 0 then
        visited
    else
        # updatedVisited = Dict.insertAll visited (Dict.fromList locationsToVisit)
        # updatedVisited = List.walk locationsToVisit visited \visitedYet, (coords, steps) ->
        #     Dict.insert visitedYet coords steps
        updatedVisited = List.concat visited locationsToVisit

        nextLocations =
            locationsToVisit # |> List.dropIf \(_coords, remainingSteps) ->
            #     remainingSteps < 1
            |> List.joinMap \(coords, remainingSteps) ->
                Grid.cardinalNeighbors gardens coords
                |> List.keepOks \(plot, plotCoords) ->
                    # hasBeenVisited = Dict.contains visited plotCoords
                    hasBeenVisited = List.any visited \(c, _s) -> c == plotCoords
                    if plot == Garden && !hasBeenVisited && remainingSteps > 0 then
                        Ok (plotCoords, remainingSteps - 1)
                    else
                        Err CantWalkToGarden
            |> Set.fromList
            |> Set.toList

        # inner updatedVisited nextLocations
        # if List.len nextLocations > 2 then
        #     updatedVisited
        # else
        innerFindReachablePlots gardens updatedVisited nextLocations

part1 = \lines ->
    stepCount = 64
    (startingCoords, gardens) = parseGardens lines
    reachablePlots = findReachablePlots gardens startingCoords stepCount

    # Num.toStr (Dict.len reachablePlots)

    # Inspect.toStr reachablePlots

    reachablePlots
    |> Dict.fromList
    |> Dict.values
    |> List.keepIf \x -> x % 2 == 0
    |> List.len
    |> Num.toStr

part2 = \_lines ->
    "TODO"

