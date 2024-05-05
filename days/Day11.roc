module [part1, part2]

import Grid
import Utils exposing [graphemes]

parseGalaxies = \lines ->
    charGrid = List.map lines graphemes
    Grid.walk charGrid [] \galaxiesSoFar, char, coords ->
        if char == "#" then
            galaxiesSoFar |> List.append coords
        else
            galaxiesSoFar

emptyRowsAndColumns = \galaxies ->
    bounds = galaxyBounds galaxies
    emptyRows =
        List.range { start: At bounds.minRow, end: At bounds.maxRow }
        |> List.dropIf \row -> List.any galaxies \g -> g.row == row
    emptyColumns =
        List.range { start: At bounds.minColumn, end: At bounds.maxColumn }
        |> List.dropIf \column -> List.any galaxies \g -> g.column == column

    { emptyRows, emptyColumns }

galaxyBounds = \galaxies ->
    inf = Num.toU64 Num.maxU32
    baseBounds = { minRow: inf, maxRow: 0, minColumn: inf, maxColumn: 0 }

    List.walk galaxies baseBounds \bounds, galaxy -> {
        minRow: Num.min bounds.minRow galaxy.row,
        maxRow: Num.max bounds.maxRow galaxy.row,
        minColumn: Num.min bounds.minColumn galaxy.column,
        maxColumn: Num.max bounds.maxColumn galaxy.column,
    }

galaxyPairDistances = \pairs, emptyRows, emptyColumns, expansionAmount ->
    List.map pairs \(galaxyA, galaxyB) ->
        (lowerRow, higherRow) = Utils.lowerAndHigher galaxyA.row galaxyB.row
        (lowerColumn, higherColumn) = Utils.lowerAndHigher galaxyA.column galaxyB.column

        expandedRows =
            List.countIf emptyRows \row -> lowerRow < row && row < higherRow
        expandedColumns =
            List.countIf emptyColumns \column -> lowerColumn < column && column < higherColumn

        (higherRow - lowerRow)
        + (expandedRows * (expansionAmount - 1))
        + (higherColumn - lowerColumn)
        + (expandedColumns * (expansionAmount - 1))

galaxyPairs = \galaxies ->
    List.walkWithIndex galaxies [] \pairs, galaxy, index ->
        galaxies
        |> List.dropFirst (index + 1)
        |> List.map \otherGalaxy -> (galaxy, otherGalaxy)
        |> List.concat pairs

part1 = \lines ->
    galaxies = parseGalaxies lines
    { emptyRows, emptyColumns } = emptyRowsAndColumns galaxies

    pairDistances =
        galaxyPairs galaxies
        |> galaxyPairDistances emptyRows emptyColumns 2

    List.sum pairDistances |> Num.toStr

part2 = \lines ->
    galaxies = parseGalaxies lines
    { emptyRows, emptyColumns } = emptyRowsAndColumns galaxies

    pairDistances =
        galaxyPairs galaxies
        |> galaxyPairDistances emptyRows emptyColumns 1_000_000

    List.sum pairDistances |> Num.toStr
