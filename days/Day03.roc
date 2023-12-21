interface Day03
    exposes [part1, part2]
    imports [Grid]

parseSchematicChar = \char ->
    when Str.toNat char is
        Ok num -> Digit num
        Err _ ->
            if char == "." then
                Empty
            else
                Symbol char

parseSchematic = \grid ->
    emptySchematic = { symbols: [], numbers: [] }

    rawSchematic = Grid.walk grid emptySchematic \{ symbols, numbers }, cell, coordinates ->
        when cell is
            Empty -> { symbols, numbers: List.append numbers [] }
            Symbol symbol ->
                { symbols: List.append symbols (symbol, coordinates), numbers: List.append numbers [] }

            Digit digit ->
                head = List.dropLast numbers 1
                last = List.last numbers |> Result.withDefault [] |> List.append (digit, coordinates)
                { symbols, numbers: List.append head last }

    consolidatedNumbers = List.keepOks rawSchematic.numbers \number ->
        if List.isEmpty number then
            Err EmptyNumber
        else
            value = List.walk number 0 \sum, (digit, _coords) -> 10 * sum + digit
            allCoords = List.map number \(_digit, coords) -> coords
            Ok (value, allCoords)

    { symbols: rawSchematic.symbols, numbers: consolidatedNumbers }

part1 = \lines ->
    grid = List.map lines \line ->
        List.map (Str.graphemes line) parseSchematicChar
    schematic = parseSchematic grid

    allSymbolCoords =
        schematic.symbols
        |> List.map \(_symbol, coords) -> coords
        |> Set.fromList
    symbolNeighbors = Set.joinMap allSymbolCoords \coords ->
        Grid.allNeighbors grid coords
        |> List.map .1
        |> Set.fromList

    partNumbers =
        schematic.numbers
        |> List.keepOks \(number, allDigitCoords) ->
            isNeighbor = List.any allDigitCoords \digitCoords ->
                Set.contains symbolNeighbors digitCoords

            if isNeighbor then Ok number else Err {}

    partNumbers
    |> List.sum
    |> Num.toStr

part2 = \lines ->
    grid = List.map lines \line ->
        List.map (Str.graphemes line) parseSchematicChar
    schematic = parseSchematic grid

    allGearCandidates =
        schematic.symbols
        |> List.keepOks \(symbol, coords) ->
            if symbol == "*" then Ok coords else Err {}
    allGearRatios =
        allGearCandidates
        |> List.keepOks \gearCoords ->
            allNeighborCoords =
                Grid.allNeighbors grid gearCoords
                |> List.map .1
            neighborNums = List.keepOks schematic.numbers \(number, allDigitCoords) ->
                numIsNeighborOfGear = List.any allNeighborCoords \neighborCoords ->
                    List.any allDigitCoords \digitCoords -> digitCoords == neighborCoords

                if numIsNeighborOfGear then Ok number else Err {}

            if List.len neighborNums == 2 then
                Ok (List.walk neighborNums 1 \product, num -> product * num)
            else
                Err WrongNumberOfNeighbors

    allGearRatios
    |> List.sum
    |> Num.toStr
