interface Day17
    exposes [part1, part2]
    imports [AStar, Coordinates, Direction, Grid]

parseCityBlock = \lines ->
    lines
    |> List.map \line ->
        Str.graphemes line
        |> List.mapTry Str.toNat
        |> Result.withDefault []
    |> List.dropIf List.isEmpty

getNeighbors = \cityBlock, cell ->
    Direction.allCardinals
    |> List.dropIf \direction ->
        direction == cell.value.0
    |> List.joinMap \direction ->
        delta = Direction.delta direction

        List.range { start: At 1, end: At 3 }
            |> List.walkUntil [] \candidates, distance ->
                lastNeighbor = candidates
                    |> List.last
                    |> Result.withDefault { coords: cell.coords, value: cell.value, distance: 0 }
                nextNeighborCoords = Coordinates.add lastNeighbor.coords delta

                when nextNeighborCoords is
                    Err _ -> Break candidates
                    Ok neighborCoords ->
                        when Grid.get cityBlock neighborCoords is
                            Err _ -> Break candidates
                            Ok neighbor ->
                                newNeighbor = { coords: neighborCoords, value: (direction, distance), distance: lastNeighbor.distance + neighbor }
                                Continue (candidates |> List.append newNeighbor)

## The Manhattan distance between two points, multiplied by 9 to emulate
## the heat loss that occurs in traversing between neighbors.
approximateDistance = \{ grid: _cityBlock, cell: { coords, value: _value }, goal } ->
    Coordinates.manhattanDistance coords goal

part1 = \lines ->
    cityBlock = parseCityBlock lines
    (height, width) = Grid.heightAndWidth cityBlock

    topLeftCorner = { row: 0, column: 0 }
    bottomRightCorner = { row: height - 1, column: width - 1 }
    minHeatLoss = AStar.searchDistance {
        grid: cityBlock,
        start: { coords: topLeftCorner, value: (Left, 0) },
        goal: bottomRightCorner,
        getNeighbors,
        heuristic: approximateDistance,
    }

    when minHeatLoss is
        Ok heatLoss -> Num.toStr heatLoss
        Err NoPathFound -> "No path to goal found"

part2 = \_lines ->
    "TODO"
