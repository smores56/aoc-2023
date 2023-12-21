interface Coordinates
    exposes [
        Coordinates,
        add,
        allNeighbors,
        cardinalNeighbors,
    ]
    imports []

Coordinates : {
    row : Nat,
    column : Nat,
}

add = \coords, delta ->
    row = delta.row + Num.toI64 coords.row
    column = delta.column + Num.toI64 coords.column

    if row >= 0 && column >= 0 then
        Ok { row: Num.toNat row, column: Num.toNat column }
    else
        Err OutOfBounds

allNeighbors = \coords ->
    List.keepOks allDeltas \delta ->
        add coords delta

cardinalNeighbors = \coords ->
    List.keepOks cardinalDeltas \delta ->
        add coords delta

allDeltas = List.concat cardinalDeltas diagonalDeltas

cardinalDeltas = [
    { row: -1, column: 0 },
    { row: 0, column: 1 },
    { row: 1, column: 0 },
    { row: 0, column: -1 },
]

diagonalDeltas = [
    { row: -1, column: 1 },
    { row: 1, column: 1 },
    { row: 1, column: -1 },
    { row: -1, column: -1 },
]
