interface Direction
    exposes [
        Direction,
        allCardinal,
        reverse,
        delta,
    ]
    imports []

Direction : [Up, Down, Left, Right]

allCardinal = [Up, Down, Left, Right]

reverse = \direction ->
    when direction is
        Up -> Down
        Down -> Up
        Left -> Right
        Right -> Left

delta = \direction ->
    when direction is
        Up -> { row: -1, column: 0 }
        Down -> { row: 1, column: 0 }
        Left -> { row: 0, column: -1 }
        Right -> { row: 0, column: 1 }
