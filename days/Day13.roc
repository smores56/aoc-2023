module [part1, part2]

import Grid
import Utils exposing [graphemes]

groupGrids = \lines ->
    lines
    |> List.walk [] \grids, line ->
        if Str.isEmpty line then
            grids |> List.append []
        else
            patternLine = parsePatternLine line
            grid =
                List.last grids
                |> Result.withDefault []
                |> List.append patternLine
            restOfGrids = List.dropLast grids 1

            List.append restOfGrids grid
    |> List.dropIf List.isEmpty

parsePatternLine = \line ->
    graphemes line
    |> List.map \c ->
        if c == "#" then Rock else Ash

findReflectionLines = \grid ->
    verticalLines = findReflectionLinesByOrientation grid SearchVertical
    horizontalLines = findReflectionLinesByOrientation grid SearchHorizontal

    List.concat verticalLines horizontalLines

findReflectionLinesByOrientation = \grid, orientation ->
    (height, width) = Grid.heightAndWidth grid
    rowIndices = List.range { start: At 0, end: Before height }
    columnIndices = List.range { start: At 0, end: Before width }

    (walkIndices, candidateIndices) =
        when orientation is
            SearchHorizontal -> (rowIndices, columnIndices |> List.dropFirst 1)
            SearchVertical -> (columnIndices, rowIndices |> List.dropFirst 1)

    validCandidates =
        walkIndices
        |> List.walkUntil candidateIndices \candidates, index ->
            listToCheckResult =
                when orientation is
                    SearchHorizontal -> Grid.getRow grid index
                    SearchVertical -> Grid.getColumn grid index
            listToCheck = listToCheckResult |> Result.withDefault []

            remainingCandidates = List.keepIf candidates \candidate ->
                isReflectionIndex listToCheck candidate

            if List.isEmpty remainingCandidates then
                Break []
            else
                Continue remainingCandidates

    when orientation is
        SearchHorizontal -> List.map validCandidates Horizontal
        SearchVertical -> List.map validCandidates Vertical

isReflectionIndex = \list, index ->
    len = List.len list
    leftwards =
        list
        |> List.reverse
        |> List.sublist { start: (len - index), len }
    rightwards =
        List.sublist list { start: index, len }

    List.map2 leftwards rightwards Pair
    |> List.all \Pair left right -> left == right

part1 = \lines ->
    grids = groupGrids lines
    gridValues =
        grids
        |> List.joinMap findReflectionLines
        |> List.map \reflectionLine ->
            when reflectionLine is
                Vertical row -> 100 * row
                Horizontal column -> column

    gridValues
    |> List.sum
    |> Num.toStr

part2 = \lines ->
    grids = groupGrids lines

    gridValues = List.map grids \grid ->
        (height, width) = Grid.heightAndWidth grid
        rowIndices = List.range { start: At 0, end: Before height }
        columnIndices = List.range { start: At 0, end: Before width }
        allCoordinates = List.joinMap rowIndices \row ->
            List.map columnIndices \column -> { row, column }

        originalReflectionLines = findReflectionLines grid

        newReflectionLine = List.walkUntil allCoordinates NoLine \_, coords ->
            updatedGrid = Grid.update grid coords \x ->
                when x is
                    Ash -> Rock
                    Rock -> Ash
            reflectionLines = findReflectionLines updatedGrid

            changedReflectionLines =
                reflectionLines
                |> List.dropIf \line -> List.contains originalReflectionLines line

            when changedReflectionLines is
                [line] -> Break (FoundLine line)
                _ -> Continue NoLine

        when newReflectionLine is
            FoundLine (Vertical row) -> 100 * row
            FoundLine (Horizontal column) -> column
            NoLine -> crash "No line found"

    gridValues
    |> List.sum
    |> Num.toStr
