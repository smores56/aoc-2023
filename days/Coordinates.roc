module [
    Coordinates,
    add,
    manhattanDistance,
    allNeighbors,
    cardinalNeighbors,
]

import Utils

Coordinates : {
    row : U64,
    column : U64,
}

add = \coords, delta ->
    row = delta.row + Num.toI64 coords.row
    column = delta.column + Num.toI64 coords.column

    if row >= 0 && column >= 0 then
        Ok { row: Num.toU64 row, column: Num.toU64 column }
    else
        Err OutOfBounds

manhattanDistance = \coordsA, coordsB ->
    (lowerRow, higherRow) = Utils.lowerAndHigher coordsA.row coordsB.row
    (lowerColumn, higherColumn) = Utils.lowerAndHigher coordsA.column coordsB.column

    (higherRow - lowerRow) + (higherColumn - lowerColumn)

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
