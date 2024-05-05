module [part1, part2]

import Coordinates
import Direction
import Grid
import Utils exposing [graphemes]

# TODO: this runs part 2 in 20 seconds (optimized), that is really inefficient

parseTile = \char ->
    when char is
        "|" -> Splitter Vertical
        "-" -> Splitter Horizontal
        "\\" -> Mirror LeftAngled
        "/" -> Mirror RightAngled
        _ -> EmptySpace

parseTiles = \lines ->
    lines
    |> List.map \line ->
        graphemes line
        |> List.map parseTile
    |> List.dropIf List.isEmpty

allBeamLocations = \tiles, startingBeams, visitedBeams ->
    nextBeams =
        startingBeams
        |> Set.toList
        |> List.joinMap \(beamCoords, beamDirection) ->
            nextBeamLocations tiles beamCoords beamDirection
            |> Result.withDefault []
        |> Set.fromList
        |> Set.difference visitedBeams

    if Set.isEmpty nextBeams then
        visitedBeams
    else
        updatedVisitedBeams = Set.union visitedBeams nextBeams
        allBeamLocations tiles nextBeams updatedVisitedBeams

nextBeamLocations = \tiles, beamCoords, direction ->
    neighborCoords <- Direction.delta direction
        |> \d -> Coordinates.add beamCoords d
        |> Result.try
    neighborTile <- Grid.get tiles neighborCoords
        |> Result.map

    nextDirections = directionsAfterTile neighborTile direction

    List.map nextDirections \nextDir -> (neighborCoords, nextDir)

directionsAfterTile = \tile, incomingDirection ->
    when tile is
        Splitter Vertical ->
            when incomingDirection is
                Left | Right -> [Up, Down]
                Up | Down -> [incomingDirection]

        Splitter Horizontal ->
            when incomingDirection is
                Up | Down -> [Left, Right]
                Left | Right -> [incomingDirection]

        Mirror RightAngled ->
            when incomingDirection is
                Up -> [Right]
                Down -> [Left]
                Left -> [Down]
                Right -> [Up]

        Mirror LeftAngled ->
            when incomingDirection is
                Up -> [Left]
                Down -> [Right]
                Left -> [Up]
                Right -> [Down]

        EmptySpace -> [incomingDirection]

part1 = \lines ->
    tiles = parseTiles lines

    numberOfTilesEnergizedFromStart tiles { row: 0, column: 0 } Right
    |> Num.toStr

part2 = \lines ->
    tiles = parseTiles lines
    (height, width) = Grid.heightAndWidth tiles
    columnIndices = List.range { start: At 0, end: Before width }
    rowIndices = List.range { start: At 0, end: Before height }

    allStartingBeams =
        [
            List.map columnIndices \column -> ({ row: height - 1, column }, Up),
            List.map columnIndices \column -> ({ row: 0, column }, Down),
            List.map rowIndices \row -> ({ row, column: width - 1 }, Left),
            List.map rowIndices \row -> ({ row, column: 0 }, Right),
        ]
        |> List.joinMap \beams -> beams

    allStartingBeams
    |> List.map \(startingCoords, startingDirection) ->
        numberOfTilesEnergizedFromStart tiles startingCoords startingDirection
    |> List.max
    |> Result.withDefault 0
    |> Num.toStr

numberOfTilesEnergizedFromStart = \tiles, startingCoords, startingDirection ->
    startingTile = Grid.get tiles startingCoords |> Result.withDefault EmptySpace
    startingBeams =
        directionsAfterTile startingTile startingDirection
        |> List.map \direction -> (startingCoords, direction)
        |> Set.fromList

    allBeamLocations tiles startingBeams startingBeams
    |> Set.map \(coords, _direction) -> coords
    |> Set.len
