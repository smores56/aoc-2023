interface Grid
    exposes [
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
        AStarCell,
        AStarNeighbor,
        aStarSearchDistance,
    ]
    imports [Coordinates, MinHeap]

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

aStarSearchDistance = \{ grid, start, goal, initialValue, heuristic, getNeighbors } ->
    inner = \queue, visited ->
        (current, restOfQueue) <- MinHeap.removeMin queue
            |> Result.mapErr \_ -> NoPathFound
            |> Result.try

        if current.coords == goal then
            Ok current.g
        else
            nextNeighbors =
                getNeighbors grid { coords: current.coords, value: current.value }
                |> List.dropIf \neighbor -> Set.contains visited (neighbor.coords, neighbor.value)
                |> List.map \neighbor ->
                    neighborG = current.g + neighbor.distance
                    neighborH = heuristic { grid, cell: { coords: neighbor.coords, value: neighbor.value }, goal }

                    {
                        value: neighbor.value,
                        coords: neighbor.coords,
                        g: neighborG,
                        h: neighborH,
                        f: neighborG + neighborH,
                    }

            updatedVisited = Set.insert visited current.coords
            updatedQueue =
                List.walk nextNeighbors restOfQueue \buildingQueue, neighbor ->
                    MinHeap.insert buildingQueue neighbor

            inner updatedQueue updatedVisited

    # inner [{ coords: start.coords, value: start.value, g: 0, h: 0, f: 0 }] (Set.empty {})

    firstCell = { coords: start, value: initialValue, g: 0, h: 0, f: 0 }
    minFValue = \cellA, cellB ->
        Num.compare cellA.f cellB.f
    initialQueue = MinHeap.fromList [firstCell] minFValue

    inner initialQueue (Set.empty {})
