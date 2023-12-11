interface Grid
    exposes [
        Grid,
        get,
        walk,
        find,
        heightAndWidth,
        neighbors,
    ]
    imports [Coordinates]

Grid a : List (List a)

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

neighbors = \grid, coords ->
    Coordinates.neighbors coords
    |> List.keepOks \neighbor ->
        get grid neighbor
        |> Result.map \n -> (n, coords)
