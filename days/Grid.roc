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
        neighbors,
        AStarCell,
        AStarNeighbor,
        aStarSearchDistance,
    ]
    imports [Coordinates]

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

neighbors = \grid, coords ->
    Coordinates.neighbors coords
    |> List.keepOks \neighbor ->
        get grid neighbor
        |> Result.map \n -> (n, neighbor)

AStarCell a : GridCell
    {
        g : Nat,
        h : Nat,
        f : Nat,
    }
    a

AStarNeighbor a : GridCell
    {
        distance : Nat,
    }
    a

AStarSearchDistanceParams a b : {
    grid : Grid a,
    start : GridCell b,
    goal : Coordinates.Coordinates,
    heuristic :
    {
        grid : Grid a,
        cell : GridCell b,
        goal : Coordinates.Coordinates,
    }
    -> Nat,
    getNeighbors : Grid a, GridCell b -> List (AStarNeighbor b),
}

aStarSearchDistance : AStarSearchDistanceParams a b -> Result Nat [NoPathFound] where b implements Bool.Eq & Hash.Hash
aStarSearchDistance = \{ grid, start, goal, heuristic, getNeighbors } ->
    inner = \queue, visited ->
        current <- List.first queue
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

            updatedVisited = Set.insert visited (current.coords, current.value)
            updatedQueue =
                List.dropFirst queue 1
                |> List.concat nextNeighbors
                |> List.sortWith \a, b -> Num.compare a.f b.f

            inner updatedQueue updatedVisited

    inner [{ coords: start.coords, value: start.value, g: 0, h: 0, f: 0 }] (Set.empty {})
