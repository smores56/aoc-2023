interface Grid
    exposes [
        Grid,
        Coordinates,
        get,
        walk,
        heightAndWidth,
        coordinateNeighbors,
        neighborCoordinates,
    ]
    imports []

Grid a : List (List a)

Coordinates : {
    row : Nat,
    column : Nat,
}

get = \grid, coords ->
    grid
    |> List.get coords.row
    |> Result.try \row ->
        List.get row coords.column

walk = \grid, startingState, accumulator ->
    List.walkWithIndex grid startingState \rowState, row, rowIndex ->
        List.walkWithIndex row rowState \state, cell, columnIndex ->
            coordinates = { row: rowIndex, column: columnIndex }
            accumulator state cell coordinates

heightAndWidth = \grid ->
    height = List.len grid
    width =
        List.get grid 0
        |> Result.map List.len
        |> Result.withDefault 0

    (height, width)

coordinateNeighbors = \{ row, column } ->
    candidates = [
        if row > 0 then Ok { row: row - 1, column } else Err OutOfBounds,
        if row > 0 then Ok { row: row - 1, column: column + 1 } else Err OutOfBounds,
        Ok { row, column: column + 1 },
        Ok { row: row + 1, column: column + 1 },
        Ok { row: row + 1, column },
        if column > 0 then Ok { row: row + 1, column: column - 1 } else Err OutOfBounds,
        if column > 0 then Ok { row, column: column - 1 } else Err OutOfBounds,
        if row > 0 && column > 0 then Ok { row: row - 1, column: column - 1 } else Err OutOfBounds,
    ]

    List.keepOks candidates \c -> c

neighborCoordinates = \grid, coords ->
    coordinateNeighbors coords
    |> List.keepOks \neighbor ->
        get grid neighbor |> Result.map \_ -> neighbor
