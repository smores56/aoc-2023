interface Grid
    exposes [
        Grid,
        get,
        getRow,
        getColumn,
        update,
        walk,
        find,
        heightAndWidth,
        allNeighbors,
        cardinalNeighbors,
        AStarCell,
        AStarNeighbor,
        aStarSearchDistance,
    ]
    imports [Coordinates]

Grid a : List (List a)

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

AStarCell a : {
    value : a,
    coords : Coordinates.Coordinates,
    g : Nat,
    h : Nat,
    f : Nat,
}

AStarNeighbor a : {
    value : a,
    coords : Coordinates.Coordinates,
    distance : Nat,
}

aStarSearchDistance :
    {
        grid : Grid a,
        start : Coordinates.Coordinates,
        goal : Coordinates.Coordinates,
        initialValue : b,
        heuristic : { grid : Grid a, value : b, coords : Coordinates.Coordinates, goal : Coordinates.Coordinates } -> Nat,
        getNeighbors : Grid a, b, Coordinates.Coordinates -> List (AStarNeighbor b),
    }
    -> Result Nat [NoPathFound]
aStarSearchDistance = \{ grid, start, goal, initialValue, heuristic, getNeighbors } ->
    inner = \queue, visited ->
        current <- List.first queue
            |> Result.mapErr \_ -> NoPathFound
            |> Result.try

        if current.coords == goal then
            Ok current.g
        else
            nextNeighbors =
                getNeighbors grid current.value current.coords
                |> List.dropIf \neighbor ->
                    Set.contains visited neighbor.coords
                |> List.map \neighbor ->
                    neighborG = current.g + neighbor.distance
                    neighborH = heuristic { grid, value: neighbor.value, coords: neighbor.coords, goal }

                    {
                        value: neighbor.value,
                        coords: neighbor.coords,
                        g: neighborG,
                        h: neighborH,
                        f: neighborG + neighborH,
                    }

            updatedVisited = Set.insert visited current.coords
            updatedQueue =
                List.dropFirst queue 1
                |> List.concat nextNeighbors
                |> List.sortWith \a, b -> Num.compare a.f b.f

            inner updatedQueue updatedVisited

    inner [{ coords: start, value: initialValue, g: 0, h: 0, f: 0 }] (Set.empty {})
