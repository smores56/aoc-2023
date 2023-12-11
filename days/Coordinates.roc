interface Coordinates
    exposes [
        Coordinates,
        add,
        neighbors,
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

neighbors = \coords ->
    deltas = [
        { row: -1, column: 0 },
        { row: -1, column: 1 },
        { row: 0, column: 1 },
        { row: 1, column: 1 },
        { row: 1, column: 0 },
        { row: 1, column: -1 },
        { row: 0, column: -1 },
        { row: -1, column: -1 },
    ]

    List.keepOks deltas \delta ->
        add coords delta
