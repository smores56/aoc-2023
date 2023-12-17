interface Day17
    exposes [part1, part2]
    imports [Grid, Utils]

parseCityBlock = \lines ->
    lines
    |> List.map \line ->
        Str.graphemes line
        |> List.mapTry Str.toNat
        |> Result.withDefault []
    |> List.dropIf List.isEmpty

getNeighbors = \cityBlock, value, coords ->
    Grid.neighbors cityBlock coords
    |> List.map \(neighbor, neighborCoords) ->
        { value: (neighbor, 0), coords: neighborCoords, distance: neighbor }
    |> List.dropIf \neighbor ->
        neighbor.value.1 > 3

## The Manhattan distance between two points, multiplied by 9 to emulate
## the heat loss that occurs in traversing between neighbors.
approximateDistance = \{ grid: _cityBlock, value: _value, coords, goal } ->
    (lowerRow, higherRow) = Utils.lowerAndHigher coords.row goal.row
    (lowerColumn, higherColumn) = Utils.lowerAndHigher coords.column goal.column

    9 * (higherRow - lowerRow + higherColumn - lowerColumn)

part1 = \lines ->
    cityBlock = parseCityBlock lines
    (height, width) = Grid.heightAndWidth cityBlock

    minHeatLoss = Grid.aStarSearchDistance {
        grid: cityBlock,
        start: { row: 0, column: 0 },
        goal: { row: height - 1, column: width - 1 },
        initialValue: (0, 0),
        getNeighbors,
        heuristic: approximateDistance,
    }

    when minHeatLoss is
        Ok heatLoss -> Num.toStr heatLoss
        Err err -> "No path to goal found: \(Inspect.toStr err)"

part2 = \_lines ->
    "TODO"
