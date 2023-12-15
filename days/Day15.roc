interface Day15
    exposes [part1, part2]
    imports []

parseInstruction = \line ->
    if Str.endsWith line "-" then
        { before: label, after: _ } <- Str.splitFirst line "-" |> Result.map

        (label, RemoveLens)
    else
        { before: label, after } <- Str.splitFirst line "=" |> Result.try
        lens <- Str.toNat after |> Result.map

        (label, AddLens lens)

hashString = \s ->
    Str.toScalars s
    |> List.walk 0 \value, c ->
        (value + c) * 17 % 256

runInstructionsOnBoxes = \instructions ->
    List.walk instructions (Dict.empty {}) \boxes, (label, instruction) ->
        hash = hashString label
        box = Dict.get boxes hash |> Result.withDefault []
        updatedBox = runInstructionOnBox box instruction label

        Dict.insert boxes hash updatedBox

runInstructionOnBox = \box, instruction, label ->
    when instruction is
        AddLens lens ->
            indexInBox = List.findFirstIndex box \(labelInBox, _boxLens) ->
                labelInBox == label
            when indexInBox is
                Ok index -> List.set box index (label, lens)
                Err _ -> List.append box (label, lens)

        RemoveLens ->
            List.dropIf box \(labelInBox, _lens) ->
                labelInBox == label

totalFocusingPower = \boxes ->
    Dict.toList boxes
    |> List.joinMap \(boxNumber, boxLenses) ->
        List.mapWithIndex boxLenses \(_boxLabel, boxLens), slotNumber ->
            Num.toNat (boxNumber + 1) * (slotNumber + 1) * boxLens
    |> List.sum

part1 = \lines ->
    line =
        List.get lines 0
        |> Result.withDefault ""
    strings = Str.split line ","

    strings
    |> List.map hashString
    |> List.sum
    |> Num.toStr

part2 = \lines ->
    line =
        List.get lines 0
        |> Result.withDefault ""
    instructions =
        Str.split line ","
        |> List.keepOks parseInstruction

    runInstructionsOnBoxes instructions
    |> totalFocusingPower
    |> Num.toStr
