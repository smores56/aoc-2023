interface Day05
    exposes [part1, part2]
    imports [Utils]

parseInput = \lines ->
    seeds <- List.get lines 0
        |> Result.try parseSeeds
        |> Result.try

    otherLines = List.dropFirst lines 2
    groupedMapLines = List.walk otherLines [] \mapLines, line ->
        if line == "" then
            List.append mapLines []
        else
            head = List.dropLast mapLines 1
            tail = List.last mapLines |> Result.withDefault []

            List.append head (List.append tail line)

    nonEmptyGroupedLines = List.keepIf groupedMapLines \ls -> !(List.isEmpty ls)

    maps <- List.mapTry nonEmptyGroupedLines parseMap |> Result.map

    (seeds, maps)

parseSeeds = \line ->
    rest <- Utils.parseLiteral line "seeds: " |> Result.try

    Str.split rest " "
    |> List.mapTry Str.toNat

parseMap = \lines ->
    { before: name, after: _ } <- List.get lines 0
        |> Result.try \line -> Str.splitFirst line " "
        |> Result.try
    { before: from, after: to } <- Str.splitFirst name "-to-"
        |> Result.try

    ranges <- List.dropFirst lines 1
        |> List.mapTry \line ->
            nums <- Str.split line " "
                |> List.mapTry Str.toNat
                |> Result.try
            when nums is
                [destStart, sourceStart, length] -> Ok { destStart, from: sourceStart, to: sourceStart + length - 1 }
                _ -> Err WrongRangeLength
        |> Result.map

    { from, to, ranges }

findLocationOfRanges = \from, ranges, maps ->
    if from == "location" then
        Ok ranges
    else
        map <- List.findFirst maps (\m -> m.from == from) |> Result.try
        mappedRanges = mapSeedRanges ranges map.ranges

        findLocationOfRanges map.to mappedRanges maps

mapSeedRanges = \seedRanges, mapRanges ->
    (remainingSeedRanges, mappedSeedRanges) = List.walk mapRanges (seedRanges, []) \(remainingRanges, mappedRanges), mapRange ->
        remainingRanges
        |> List.walk ([], mappedRanges) \(rmRs, mRs), remainingRange ->
            overlapsMapRange =
                mapRange.from <= remainingRange.to && mapRange.to >= remainingRange.from

            if overlapsMapRange then
                result = buildMappedSeedRanges remainingRange mapRange
                (rmRs |> List.concat result.remainingSeedRanges, mRs |> List.append result.mappedSeedRange)
            else
                (rmRs |> List.append remainingRange, mRs)

    List.concat remainingSeedRanges mappedSeedRanges

buildMappedSeedRanges = \seedRange, mapRange ->
    mappedCenterRange = {
        from: (Num.max seedRange.from mapRange.from) + mapRange.destStart - mapRange.from,
        to: (Num.min seedRange.to mapRange.to) + mapRange.destStart - mapRange.from,
    }

    includeLeft = seedRange.from < mapRange.from && seedRange.to >= mapRange.from
    includeRight = seedRange.to > mapRange.to && seedRange.from <= mapRange.to

    remainingSeedRanges =
        [
            if includeLeft then Ok { from: seedRange.from, to: mapRange.from - 1 } else Err {},
            if includeRight then Ok { from: mapRange.to + 1, to: seedRange.to } else Err {},
        ]
        |> List.keepOks \x -> x

    { mappedSeedRange: mappedCenterRange, remainingSeedRanges }

part1 = \lines ->
    (seeds, maps) = parseInput lines |> Result.withDefault ([], [])
    seedRanges = seeds |> List.map \seed -> { from: seed, to: seed }
    seedLocations =
        findLocationOfRanges "seed" seedRanges maps
        |> Result.withDefault []

    seedLocations
    |> List.map \{ from, to: _ } -> from
    |> List.min
    |> Result.map Num.toStr
    |> Result.withDefault "No locations found"

part2 = \lines ->
    (seedInstructions, maps) = parseInput lines |> Result.withDefault ([], [])
    seedRanges =
        List.chunksOf seedInstructions 2
        |> List.keepOks \instructions ->
            when instructions is
                [base, length] -> Ok { from: base, to: base + length - 1 }
                _ -> Err {}

    seedLocations =
        findLocationOfRanges "seed" seedRanges maps
        |> Result.withDefault []

    seedLocations
    |> List.map \{ from, to: _ } -> from
    |> List.min
    |> Result.map Num.toStr
    |> Result.withDefault "No locations found"
