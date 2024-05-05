module [
    Grid,
    GridCell,
    get,
    getRow,
    getColumn,
    update,
    walk,
    find,
    heightAndWidth,
    allNeighbors,
    cardinalNeighbors,
]

import Coordinates

Grid a : List (List a)

GridCell a : {
    value : a,
    coords : Coordinates.Coordinates,
}

get = \grid, coords ->
    grid
    |> List.get coords.row
    |> Result.try \row ->
        List.get row coords.column

getRow = \grid, rowIndex ->
    List.get grid rowIndex

getColumn = \grid, columnIndex ->
    List.mapTry grid \row -> List.get row columnIndex

update = \grid, coords, updater ->
    grid
    |> List.update coords.row \row ->
        row |> List.update coords.column updater

walk = \grid, startingState, accumulator ->
    List.walkWithIndex grid startingState \rowState, row, rowIndex ->
        List.walkWithIndex row rowState \state, cell, columnIndex ->
            coordinates = { row: rowIndex, column: columnIndex }
            accumulator state cell coordinates

find = \grid, predicate ->
    walk grid NotFound \wasFound, cell, coordinates ->
        if predicate cell && wasFound == NotFound then
            Found cell coordinates
        else
            wasFound

heightAndWidth = \grid ->
    height = List.len grid
    width =
        List.get grid 0
        |> Result.map List.len
        |> Result.withDefault 0

    (height, width)

allNeighbors = \grid, coords ->
    Coordinates.allNeighbors coords
    |> List.keepOks \neighbor ->
        get grid neighbor
        |> Result.map \n -> (n, neighbor)

cardinalNeighbors = \grid, coords ->
    Coordinates.cardinalNeighbors coords
    |> List.keepOks \neighbor ->
        get grid neighbor
        |> Result.map \n -> (n, neighbor)
