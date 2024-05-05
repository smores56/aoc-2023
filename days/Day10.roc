module [part1, part2]

import Utils exposing [graphemes]

import Grid
import Coordinates

reverseDirection = \direction ->
    when direction is
        Up -> Down
        Down -> Up
        Left -> Right
        Right -> Left

allDirections = [Up, Down, Left, Right]

directionsPipesFace =
    Dict.empty {}
    |> Dict.insert "|" [Up, Down]
    |> Dict.insert "-" [Left, Right]
    |> Dict.insert "L" [Up, Right]
    |> Dict.insert "J" [Up, Left]
    |> Dict.insert "7" [Down, Left]
    |> Dict.insert "F" [Down, Right]
    |> Dict.insert "S" [Up, Down, Left, Right]

deltaForDirection = \direction ->
    when direction is
        Up -> { row: -1, column: 0 }
        Down -> { row: 1, column: 0 }
        Left -> { row: 0, column: -1 }
        Right -> { row: 0, column: 1 }

getNeighbor = \pipes, coords, inDirection ->
    delta = deltaForDirection inDirection
    neighborCoords <- Coordinates.add coords delta
        |> Result.try
    neighborDirections <- Grid.get pipes neighborCoords
        |> Result.map

    { inDirection, coords: neighborCoords, directions: neighborDirections }

parseInput = \lines ->
    charGrid = List.map lines graphemes
    pipes = parsePipes charGrid

    when Grid.find charGrid \c -> c == "S" is
        NotFound -> Err MissingStartPosition
        Found _startChar startingCoords -> Ok (pipes, startingCoords)

parsePipes = \charGrid ->
    List.map charGrid \chars ->
        List.map chars \char ->
            directionsPipesFace
            |> Dict.get char
            |> Result.withDefault []

pipeNeighbors = \pipes, coords ->
    directionsFaced <- Grid.get pipes coords |> Result.map

    List.keepOks directionsFaced \direction ->
        delta = deltaForDirection direction
        neighborCoords <- Coordinates.add coords delta |> Result.try
        neighbor <- Grid.get pipes neighborCoords |> Result.try

        otherWay = reverseDirection direction
        if List.contains neighbor otherWay then
            Ok neighborCoords
        else
            Err NeighborNotConnected

searchPipesBreadthFirst = \pipes, startingCoords ->
    inner = \queue, visited ->
        when List.first queue is
            Err _ -> visited
            Ok (pipeCoords, distanceFromStart) ->
                neighbors =
                    pipeNeighbors pipes pipeCoords
                    |> Result.withDefault []
                unvisitedNeighbors =
                    neighbors
                    |> List.dropIf \neighbor -> Dict.contains visited neighbor
                    |> List.map \neighbor -> (neighbor, distanceFromStart + 1)

                restOfQueue = List.dropFirst queue 1
                updatedQueue = List.concat restOfQueue unvisitedNeighbors
                updatedVisited = Dict.insert visited pipeCoords distanceFromStart

                inner updatedQueue updatedVisited

    inner [(startingCoords, 0)] (Dict.empty {})

coordColors = \pipes, allPipeCoords ->
    compareColors = \(_, leftColor), (_, rightColor) ->
        when (leftColor, rightColor) is
            (HasColor _, NoColor _) -> LT
            (NoColor _, HasColor _) -> GT
            (HasColor l, HasColor r) -> Num.compare l r
            (NoColor l, NoColor r) -> Num.compare l r

    inner = \queue, colors ->
        when List.first queue is
            Err _ -> colors
            Ok (coords, color) ->
                unvisitedNeighbors =
                    allDirections
                    |> List.keepOks \direction -> getNeighbor pipes coords direction
                    |> List.map \neighbor ->
                        neighborIsEmpty = !(List.contains allPipeCoords neighbor.coords)
                        newColor =
                            when color is
                                HasColor currentColor if neighborIsEmpty -> HasColor currentColor
                                NoColor prevColor if neighborIsEmpty -> HasColor (prevColor + 1)
                                HasColor currentColor -> NoColor currentColor
                                NoColor prevColor ->
                                    otherWay = reverseDirection neighbor.inDirection
                                    if List.contains neighbor.directions otherWay then
                                        NoColor prevColor
                                    else
                                        NoColor (prevColor + 1)

                        (neighbor.coords, newColor)
                    |> List.dropIf \(neighborCoords, neighborColor) ->
                        Dict.contains colors neighborCoords || List.contains queue (neighborCoords, neighborColor)

                updatedQueue =
                    queue
                    |> List.dropFirst 1
                    |> List.concat unvisitedNeighbors
                    |> List.sortWith compareColors
                updatedColors =
                    colors
                    |> Dict.insert coords color

                inner updatedQueue updatedColors

    startingCoords = { row: 0, column: 0 }
    startingColor = if List.contains allPipeCoords startingCoords then NoColor 0 else HasColor 0

    inner [(startingCoords, startingColor)] (Dict.empty {})

part1 = \_lines ->
    # (pipes, startingCoords) =
    #     parseInput lines |> Result.withDefault ([], { row: 0, column: 0 })

    # visitedDistances = searchPipesBreadthFirst pipes startingCoords

    # visitedDistances
    # |> Dict.values
    # |> List.max
    # |> Result.withDefault 0
    # |> Num.toStr

    "TODO"

part2 = \lines ->
    (pipes, startingCoords) =
        parseInput lines |> Result.withDefault ([], { row: 0, column: 0 })

    visitedDistances = searchPipesBreadthFirst pipes startingCoords
    allPipeCoords = Dict.keys visitedDistances

    coordColors pipes allPipeCoords
    |> Dict.values
    |> List.countIf \color ->
        when color is
            HasColor c -> c % 2 == 1
            NoColor _prevColor -> Bool.false
    |> Num.toStr

# coordColors pipes allPipeCoords
# |> Dict.keepIf \(_coords, color) ->
#     when color is
#         HasColor c -> c % 2 == 1
#         NoColor _prevColor -> Bool.false
# |> Inspect.toStr

# ...........
# .S-------7.
# .|F-----7|.
# .||.....||.
# .||.....||.
# .|L-7.F-J|.
# .|..|.|..|.
# .L--J.L--J.
# ...........
