interface Day24
    exposes [part1, part2]
    imports []

parseCoordinates = \line ->
    parsingResult = 
        line
        |> Str.split ","
        |> List.map Str.trim
        |> List.mapTry Str.toF64

    when parsingResult is
        Ok ([x, y, z]) -> Ok { x, y, z }
        _ -> Err InvalidCoordinates

parseHail = \line ->
    { before, after } <- Str.splitFirst line " @ "
        |> Result.try
    position <- parseCoordinates before
        |> Result.try
    velocity <- parseCoordinates after
        |> Result.map

    { position, velocity }

addCoords = \a, b ->
    { x: a.x + b.x, y: a.y + b.y, z: a.z + b.z }

## Point of Intersection (x, y) = ((b1×c2 − b2×c1)/(a1×b2 − a2×b1), (c1×a2 − c2×a1)/(a1×b2 − a2×b1))
intersectionOfTwoHailstonesXY = \hail1, hail2 ->
    coeff1 <- coefficientsOfLineFromHail hail1
        |> Result.try
    coeff2 <- coefficientsOfLineFromHail hail2
        |> Result.try

    x <- (coeff1.b * coeff2.c - coeff2.b * coeff1.c)
        |> Num.divChecked (coeff1.a * coeff2.b - coeff2.a * coeff1.b)
        |> Result.try
    y <- (coeff1.b * coeff2.c - coeff2.b * coeff1.c)
        |> Num.divChecked (coeff1.a * coeff2.b - coeff2.a * coeff1.b)
        |> Result.map

    { x, y }

coefficientsOfLineFromHail = \hail ->
    a = hail.position
    b = addCoords hail.position hail.velocity
    slope <- Num.divChecked (b.y - a.y) (b.x - a.x)
        |> Result.map
    intercept = a.y - slope * a.x

    { a: slope, b: -1.0, c: intercept }

part1 = \_lines ->
    # maxX = 200_000_000_000_000.0
    # maxY = 400_000_000_000_000.0
    # allHailstones = List.keepOks lines parseHail
    # hailstoneCount = List.len allHailstones

    # allHailstones
    # |> List.mapWithIndex \hail1, index ->
    #     allHailstones
    #     |> List.sublist { start: index + 1, len: hailstoneCount }
    #     |> List.keepOks \hail2 ->
    #         when intersectionOfTwoHailstonesXY hail1 hail2 is
    #             # Ok { x, y } -> x < maxX && y < maxY
    #             # Ok { x, y } -> x >= 0.0 && x <= maxX && y >= 0.0 && y <= maxY
    #             Ok { x, y } -> Ok "\(Num.toStr x), \(Num.toStr y)"
    #             Err _ -> Err {}
    #     |> Str.joinWith "\n"
    # # |> List.sum
    # # |> Num.toStr
    # |> Str.joinWith "\n"
    # |> Str.withPrefix "\n"

    "TODO"

part2 = \_lines ->
    "TODO"

# 19, 13, 30 @ -2, 1, -2

# Hailstone A: 19, 13, 30 @ -2, 1, -2
# Hailstone B: 18, 19, 22 @ -1, -1, -2
# Hailstones' paths will cross inside the test area (at x=14.333, y=15.333).

