interface Day10
    exposes [part1, part2]
    imports [Grid, Coordinates]

reverseDirection = \direction ->
    when direction is
        Up -> Down
        Down -> Up
        Left -> Right
        Right -> Left

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

parseInput = \lines ->
    charGrid = List.map lines Str.graphemes
    pipes = parsePipes charGrid

    when Grid.find charGrid \c -> c == "S" is
        NotFound -> Err MissingStartPosition
        Found _startChar startingCoords -> Ok { pipes, startingCoords }

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

# coordColors = \pipes, allPipeCoords ->
#     inner = \queue, colors ->
#         when List.first queue is
#             Err _ -> colors
#             Ok (coords, color) ->
#                 directionsFaced = Grid.get pipes coords
#                     |> Result.withDefault []
#                 unvisitedNeighbors =
#                     [Up, Down, Left, Right]
#                     |> List.keepOks \direction ->
#                         delta = deltaForDirection direction
#                         neighborCoords <- Coordinates.add coords delta
#                             |> Result.try
#                         neighborDirections <- Grid.get pipes neighborCoords
#                             |> Result.map

#                         newColor = if List.isEmpty neighborDirections then
#                             when color is
#                                 HasColor currentColor -> HasColor currentColor
#                                 NoColor prevColor -> HasColor (prevColor + 1)
#                         else
#                             otherWay = reverseDirection direction
#                             # if List.contains allPipeCoords c then
#                             # else
#                             #     Err Whoops
#                             when color is
#                                 HasColor currentColor -> NoColor currentColor
#                                 NoColor prevColor ->
#                                     # connected
#                                     if List.contains neighborDirections otherWay then
#                                         HasColor 1
#                                     else
#                                         123

#                         (neighborCoords, newColor)
#                     |> List.dropIf \(_neighbor, neighborCoords) ->
#                         Dict.contains colors neighborCoords

#                 updatedQueue = queue
#                     |> List.dropFirst 1
#                     |> List.concat unvisitedNeighbors
#                 updatedColors = colors
#                     |> Dict.insert coords color

#                 inner updatedQueue updatedColors

#     inner [({ row: 0, column: 0 }, HasColor 0)] (Dict.empty {})

part1 = \lines ->
    { pipes, startingCoords } =
        parseInput lines
        |> Result.withDefault { pipes: [], startingCoords: { row: 0, column: 0 } }

    visitedDistances = searchPipesBreadthFirst pipes startingCoords

    visitedDistances
    |> Dict.values
    |> List.max
    |> Result.withDefault 0
    |> Num.toStr

part2 = \_lines ->
    # { pipes, startingCoords } =
    #     parseInput lines
    #     |> Result.withDefault { pipes: [], startingCoords: { row: 0, column: 0 } }

    # visitedDistances = searchPipesBreadthFirst pipes startingCoords
    # allPipeCoords = Dict.keys visitedDistances

    # coordColors pipes allPipeCoords
    # |> Dict.values
    # |> List.countIf \color ->
    #     when color is
    #         HasColor c -> c % 2 == 1
    #         NoColor _prevColor -> Bool.false
    # |> Num.toStr

    "TODO"
