module [part1, part2]

import Grid
import Utils exposing [graphemes]

parseRock = \char ->
    when char is
        "O" -> Ok RoundRock
        "#" -> Ok CubeRock
        "." -> Ok EmptySpace
        _ -> Err InvalidRock

parseRockGrid = \lines ->
    List.keepOks lines \line ->
        graphemes line
        |> List.mapTry parseRock
    |> List.dropIf List.isEmpty

shiftRocksInDirection = \rocks, direction ->
    allShiftedCoords =
        shiftedRockCoords rocks direction
        |> Set.fromList

    List.mapWithIndex rocks \rockRow, rowIndex ->
        List.mapWithIndex rockRow \rock, columnIndex ->
            if rock == CubeRock then
                CubeRock
            else if Set.contains allShiftedCoords { row: rowIndex, column: columnIndex } then
                RoundRock
            else
                EmptySpace

shiftedRockCoords = \rocks, direction ->
    rockLists =
        if direction == Left || direction == Right then
            rocks
        else
            (_height, width) = Grid.heightAndWidth rocks
            columnIndices = List.range { start: At 0, end: Before width }

            List.keepOks columnIndices \columnIndex ->
                Grid.getColumn rocks columnIndex

    rockLists
    |> List.mapWithIndex Indexed
    |> List.joinMap \Indexed rockList index ->
        length = List.len rockList
        orientedList =
            when direction is
                Up | Left -> rockList
                Down | Right -> List.reverse rockList
        rockGroups = groupRocksInList orientedList

        List.joinMap rockGroups \group ->
            allCoordsForGroup group { index, direction, length }

allCoordsForGroup = \(groupSize, startingIndex), context ->
    List.range { start: At startingIndex, end: Before (startingIndex + groupSize) }
    |> List.map \otherIndex ->
        when context.direction is
            Up -> { row: otherIndex, column: context.index }
            Down -> { row: context.length - 1 - otherIndex, column: context.index }
            Left -> { row: context.index, column: otherIndex }
            Right -> { row: context.index, column: context.length - 1 - otherIndex }

groupRocksInList = \rocks ->
    List.walkWithIndex rocks [] \groups, rock, index ->
        when rock is
            EmptySpace -> groups
            CubeRock -> List.append groups (0, index + 1)
            RoundRock ->
                restOfGroups = List.dropLast groups 1
                (rockCount, startingIndex) =
                    List.last groups
                    |> Result.withDefault (0, 0)

                List.append restOfGroups (rockCount + 1, startingIndex)

findRockShiftingCycle = \rocks, cycleNumber, history ->
    when Dict.get history rocks is
        Ok previousCycleNumber ->
            { from: previousCycleNumber, to: cycleNumber, rocks }

        Err _ ->
            shiftedRocks = runFullRockCycle rocks
            updatedHistory = Dict.insert history rocks cycleNumber

            findRockShiftingCycle shiftedRocks (cycleNumber + 1) updatedHistory

cycleRocksMultipleTimes = \rocks, remainingCycles ->
    if remainingCycles == 0 then
        rocks
    else
        shiftedRocks = runFullRockCycle rocks
        cycleRocksMultipleTimes shiftedRocks (remainingCycles - 1)

getRoundRockCoords = \rocks ->
    Grid.walk rocks [] \roundCoords, rock, coords ->
        if rock == RoundRock then
            List.append roundCoords coords
        else
            roundCoords

runFullRockCycle = \rocks ->
    allDirections = [Up, Left, Down, Right]
    List.walk allDirections rocks \shifted, direction ->
        shiftRocksInDirection shifted direction

part1 = \lines ->
    rocks = parseRockGrid lines
    (height, _width) = Grid.heightAndWidth rocks

    shiftedRocks = shiftRocksInDirection rocks Up

    getRoundRockCoords shiftedRocks
    |> List.map \coords -> height - coords.row
    |> List.sum
    |> Num.toStr

part2 = \lines ->
    rocks = parseRockGrid lines
    totalCycles = 1_000_000_000
    (height, _width) = Grid.heightAndWidth rocks

    cycle = findRockShiftingCycle rocks 0 (Dict.empty {})
    cycleLength = cycle.to - cycle.from
    allRemainingCycles = totalCycles - cycle.to
    neededRemainingCycles = allRemainingCycles % cycleLength

    shiftedRocks = cycleRocksMultipleTimes cycle.rocks neededRemainingCycles

    getRoundRockCoords shiftedRocks
    |> List.map \coords -> height - coords.row
    |> List.sum
    |> Num.toStr
