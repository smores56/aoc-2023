module [part1, part2]

import Utils

Block : {
    x : U64,
    y : U64,
    z : U64,
}

parseBrickEnds : Str -> Result { start : Block, end : Block } [InvalidCoordinates, NotFound]
parseBrickEnds = \line ->
    { before, after } <- Str.splitFirst line "~"
        |> Result.try
    start <- parseCoordinates before
        |> Result.try
    end <- parseCoordinates after
        |> Result.map

    { start, end }

parseCoordinates : Str -> Result Block [InvalidCoordinates]
parseCoordinates = \line ->
    parseResult = line
        |> Str.split ","
        |> List.mapTry Str.toU64

    when parseResult is
        Ok ([x, y, z]) -> Ok { x, y, z }
        _ -> Err InvalidCoordinates

Brick : {
    blocks : List Block,
    supportedBy : Set Block,
}

brickFromEnds : { start : Block, end : Block } -> Brick
brickFromEnds = \{ start, end } ->
    if start.x != end.x then
        (minX, maxX) = Utils.lowerAndHigher start.x end.x
        List.range { start: At minX, end: At maxX }
        |> List.map \x -> { x, y: start.y, z: start.z }
        |> \blocks -> { blocks, supportedBy: Set.empty {} }
    else if start.y != end.y then
        (minY, maxY) = Utils.lowerAndHigher start.y end.y
        List.range { start: At minY, end: At maxY }
        |> List.map \y -> { x: start.x, y, z: start.z }
        |> \blocks -> { blocks, supportedBy: Set.empty {} }
    else if start.z != end.z then
        (minZ, maxZ) = Utils.lowerAndHigher start.z end.z
        List.range { start: At minZ, end: At maxZ }
        |> List.map \z -> { x: start.x, y: start.y, z }
        |> \blocks -> { blocks, supportedBy: Set.empty {} }
    else
        { blocks: [start], supportedBy: Set.empty {} }

brickZ : Brick, [Top, Bottom] -> U64
brickZ = \brick, side ->
    zCoords = List.map brick.blocks .z
    when side is
        Top -> List.max zCoords |> Result.withDefault 0
        Bottom -> List.min zCoords |> Result.withDefault 0

brickFootprint : Brick, [Top, Bottom] -> (U64, List Block)
brickFootprint = \brick, side ->
    zOfSide = brickZ brick side
    (zOfSide, brick.blocks |> List.keepIf \b -> b.z == zOfSide)

applyGravityToBricks : List Brick -> Result (List Brick) [ListWasEmpty]
applyGravityToBricks = \bricks ->
    sortedBricks = List.sortWith bricks \a, b ->
        Num.compare (brickZ a Bottom) (brickZ b Bottom)
    firstBrick <- List.first sortedBricks
        |> Result.map

    applyGravityToBricksInner [firstBrick] (List.dropFirst sortedBricks 1)

applyGravityToBricksInner : List Brick, List Brick -> List Brick
applyGravityToBricksInner = \bricksSoFar, remainingBricks ->
    when List.first remainingBricks is
        Err _ -> bricksSoFar
        Ok nextBrick ->
            (nextBrickZ, nextBrickFootprint) = brickFootprint nextBrick Bottom
            highestBlocksUnder =
                nextBrickFootprint
                |> List.keepOks \coords ->
                    bricksSoFar 
                    |> List.keepOks \otherBrick ->
                        otherBrickZ = brickZ otherBrick Bottom
                        blockAtLevel = { coords & z: otherBrickZ }
                        if List.contains otherBrick.blocks blockAtLevel then
                            Ok blockAtLevel
                        else
                            Err BrickNotInColumn
                    |> List.last

            maxZUnderBrick = highestBlocksUnder
                |> List.map .z
                |> List.max
                |> Result.withDefault 0
            supportedBy =
                highestBlocksUnder
                |> List.keepIf \block -> block.z == maxZUnderBrick
                |> Set.fromList

            zOffset = nextBrickZ - maxZUnderBrick
            fallenBlocks = nextBrick.blocks
                |> List.map \block -> { block & z: block.z - zOffset + 1 }

            bricksSoFar
            |> List.append { blocks: fallenBlocks, supportedBy }
            |> applyGravityToBricksInner (List.dropFirst remainingBricks 1)

# A brick can be disintegrated if no bricks are only supported by this brick
brickCouldBeDisintegrated : Brick, List Brick -> Bool
brickCouldBeDisintegrated = \brick, allBricks ->
    blockSet = Set.fromList brick.blocks

    List.all allBricks \otherBrick ->
        doesntSupportOtherBrick = otherBrick.supportedBy
            |> Set.intersection blockSet
            |> Set.isEmpty
        otherBrickWouldStillBeSupported = otherBrick.supportedBy
            |> Set.difference blockSet
            |> Set.isEmpty
            |> Bool.not

        doesntSupportOtherBrick || otherBrickWouldStillBeSupported 

part1 : List Str -> Str
part1 = \lines ->
    # 499 is too high
    brickEnds = when lines |> List.dropIf Str.isEmpty |> List.mapTry parseBrickEnds is
        Ok ends -> ends
        Err err -> crash "Failed to parse all brick ends: \(Inspect.toStr err)"
    bricks = List.map brickEnds brickFromEnds

    fallenBricks = applyGravityToBricks bricks
        |> Result.withDefault []

    fallenBricks
    |> List.countIf \brick -> brickCouldBeDisintegrated brick fallenBricks
    |> Num.toStr

    # fallenBricks
    # |> List.map Inspect.toStr
    # |> Str.joinWith "\n"
    # |> Str.withPrefix "\n"

part2 : List Str -> Str
part2 = \_lines ->
    "TODO"
