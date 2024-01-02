interface Day23
    exposes [part1, part2]
    imports [Coordinates, Direction, Grid]

parseSlope = \char ->
    when char is
        "^" -> Ok Up
        "v" -> Ok Down
        "<" -> Ok Left
        ">" -> Ok Right
        _ -> Err InvalidSlope

parseCell = \char ->
    when parseSlope char is
        Ok slope -> Slope slope
        Err _ ->
            if char == "." then
                Path
            else
                Tree

parseForest = \lines ->
    lines
    |> List.dropIf Str.isEmpty
    |> List.map \line ->
        line
        |> Str.graphemes
        |> List.map parseCell

findStartAndEndCoords = \forest ->
    findFirstPathInRow = \row ->
        List.findFirstIndex row \cell -> cell == Path

    lastRow = List.len forest - 1
    startColumn <- Grid.getRow forest 0
        |> Result.try findFirstPathInRow
        |> Result.try
    endColumn <- Grid.getRow forest lastRow
        |> Result.try findFirstPathInRow
        |> Result.map

    ({ row: 0, column: startColumn }, { row: lastRow, column: endColumn })

findForestPaths = \forest, startingCoords ->
    buildGraphFromForest forest [(startingCoords, [])] (Dict.empty {}) (Set.empty {})

buildGraphFromForest = \forest, queue, edges, visited ->
    when List.first queue is
        Err ListWasEmpty -> edges
        Ok (startCoords, restOfCoords) ->
            lastCoords =
                List.last restOfCoords
                |> Result.withDefault startCoords
            allNeighbors =
                Grid.cardinalNeighbors forest lastCoords
                |> List.dropIf \(neighbor, _neighborCoords) ->
                    neighbor == Tree
            unvisitedNeighbors =
                List.dropIf allNeighbors \(_neighbor, neighborCoords) ->
                    Set.contains visited neighborCoords

            (queueAdditions, newEdges, updatedVisited) =
                if List.len allNeighbors > 2 then
                    neighborsToVisit = accessiblePathsFromJunction forest lastCoords
                    visitedNeighbors = neighborsToVisit |> List.joinMap .1

                    (
                        neighborsToVisit,
                        [{ end: lastCoords, len: List.len restOfCoords }],
                        visited |> Set.union (Set.fromList visitedNeighbors)
                    )
                else
                    when List.first unvisitedNeighbors is
                        Ok (_neighbor, neighborCoords) ->
                            (
                                [(startCoords, restOfCoords |> List.append neighborCoords)],
                                [],
                                visited |> Set.insert lastCoords
                            )

                        Err _ ->
                            (
                                [],
                                [{ end: lastCoords, len: List.len restOfCoords }],
                                visited |> Set.insert lastCoords
                            )

            updatedQueue = List.concat queueAdditions (List.dropFirst queue 1)
            updatedEdges =
                if List.isEmpty newEdges then
                    edges
                else
                    startEdges =
                        edges
                        |> Dict.get startCoords
                        |> Result.withDefault []
                        |> List.concat newEdges
                        |> List.dropIf \edge -> edge.end == startCoords
                    Dict.insert edges startCoords startEdges

            buildGraphFromForest forest updatedQueue updatedEdges updatedVisited

accessiblePathsFromJunction = \forest, coords ->
    Direction.allCardinals
    |> List.keepOks \direction ->
        delta = Direction.delta direction
        neighborCoords <- Coordinates.add coords delta
            |> Result.try
        nextNeighborCoords <- Coordinates.add neighborCoords delta
            |> Result.try
        neighbor <- Grid.get forest neighborCoords
            |> Result.try

        when neighbor is
            Tree -> Err CannotTraverseTree
            Path -> Ok (coords, [neighborCoords, nextNeighborCoords])
            Slope slopeDirection ->
                if slopeDirection == direction then
                    Ok (coords, [neighborCoords, nextNeighborCoords])
                else
                    Err SlopeFacingWrongWay

longestPathLength = \edges, start, goal, visited ->
    if start == goal then
        Ok 0
    else
        updatedVisited =
            visited
            |> Set.insert start
        neighbors =
            edges
            |> Dict.get start
            |> Result.withDefault []

        neighbors
        |> List.keepOks \neighbor ->
            len <- longestPathLength edges neighbor.end goal updatedVisited
                |> Result.map

            len + neighbor.len
        |> List.max
        |> Result.mapErr \_err -> NoLongestPathFound

part1 = \lines ->
    forest = parseForest lines
    (startCoords, endCoords) =
        when findStartAndEndCoords forest is
            Ok pair -> pair
            Err _ -> crash "Failed to find start and end coordinates"

    paths = findForestPaths forest startCoords

    when longestPathLength paths startCoords endCoords (Set.empty {}) is
        Ok length -> Num.toStr length
        Err err -> "Failed to find longest path through forest: \(Inspect.toStr err)"

part2 = \lines ->
    forest = parseForest lines
        # |> List.map \row ->
        #     List.map row \cell ->
        #         when cell is
        #             Tree -> Tree
        #             _other -> Path

    (startCoords, endCoords) =
        when findStartAndEndCoords forest is
            Ok pair -> pair
            Err _ -> crash "Failed to find start and end coordinates"

    paths = findForestPaths forest startCoords
    bidirectionalPaths = Dict.walk paths paths \pathsSoFar, start, ends ->
        List.walk ends pathsSoFar \innerPathsSoFar, { end, len } ->
            allEnds = Dict.get innerPathsSoFar end
                |> Result.withDefault []
                |> List.append { end: start, len }
            Dict.insert pathsSoFar end allEnds

    # Inspect.toStr bidirectionalPaths

    when longestPathLength bidirectionalPaths startCoords endCoords (Set.empty {}) is
        Ok length -> Num.toStr length
        Err err -> "Failed to find longest path through forest: \(Inspect.toStr err)"
