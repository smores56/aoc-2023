module [part1, part2]

# parseDirection = \char ->
#     when char is
#         "U" -> Ok Up
#         "D" -> Ok Down
#         "L" -> Ok Left
#         "R" -> Ok Right
#         _ -> Err InvalidDirection

# parseColor = \_chars ->
#     Ok { rgb: 123 }

# R 6 (#70c710)
# parseDigInstruction = \line ->
#     chars = graphemes line
#     direction <- List.get chars 0
#         |> Result.try parseDirection
#         |> Result.try
#     distance <- List.get chars 2
#         |> Result.try Str.toU64
#         |> Result.try
#     color <- List.dropFirst chars 5
#         |> List.dropLast 1
#         |> parseColor
#         |> Result.map

#     { direction, distance, color }

part1 = \_lines ->
    # digInstructions = List.keepOks lines parseDigInstruction

    "TODO"

part2 = \_lines ->
    "TODO"
