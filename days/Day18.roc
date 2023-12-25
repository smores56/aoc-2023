interface Day18
    exposes [part1, part2]
    imports []

parseDirection = \char ->
    when char is
        "U" -> Ok Up
        "D" -> Ok Down
        "L" -> Ok Left
        "R" -> Ok Right
        _ -> Err InvalidDirection

parseColor = \chars ->
    Ok { rgb: 123 }

# R 6 (#70c710)
parseDigInstruction = \line ->
    chars = Str.graphemes line
    direction <- List.get chars 0
        |> Result.try parseDirection
        |> Result.try
    distance <- List.get chars 2
        |> Result.try Str.toNat
        |> Result.try
    color <- List.dropFirst chars 5
        |> List.dropLast 1
        |> parseColor
        |> Result.map

    { direction, distance, color }

part1 = \lines ->
    digInstructions = List.keepOks lines parseDigInstruction

    "TODO"

part2 = \_lines ->
    "TODO"
