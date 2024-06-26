app [main] {
    days: "days/main.roc",
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
}

import pf.Stdout
import pf.Stderr
import pf.File
import pf.Http
import pf.Task
import pf.Path
import pf.Dir
import pf.Arg
import pf.Utc
import days.Day01
import days.Day02
import days.Day03
import days.Day04
import days.Day05
import days.Day06
import days.Day07
import days.Day08
import days.Day09
import days.Day10
import days.Day11
import days.Day12
import days.Day13
import days.Day14
import days.Day15
import days.Day16
import days.Day17
import days.Day18
import days.Day19
import days.Day20
import days.Day21
import days.Day22
import days.Day23
import days.Day24
import days.Day25

aocYear = 2023

main =
    # error <- findSolutionsForDay DAY |> Task.onErr
    error <- getDayFromArgs
        |> Task.await findSolutionsForDay
        |> Task.onErr

    message =
        when error is
            DayNotImplemented day -> "Day \(Num.toStr day) has not been implemented yet"
            InvalidDayArg -> "The day argument must be a positive integer"
            MissingDayArg -> "You must pass a day of the month to run solutions for"
            MissingSessionToken -> "You have not saved your session token to .session, check the README for directions"
            FailedToCacheInput -> "Failed to save loaded input to cache"
            FailedToCreateCacheDir -> "Failed to create \(inputCacheDir) cache directory"
            FailedToFetchInput httpErr ->
                "Failed to fetch input from the Advent of Code site: \(Inspect.toStr httpErr)"
            other -> "Unknown error occurred: $(Inspect.toStr other)"

    Stderr.line message

findSolutionsForDay = \day ->
    _ <- Stdout.line "Advent of Code \(Num.toStr aocYear) solutions for day \(Num.toStr day):" |> Task.await

    inputLines <- loadInput day |> Task.await
    (part1, part2) <- solutionsForDay day |> Task.fromResult |> Task.await

    _ <- runSolutionForPart inputLines part1 1 |> Task.await
    _ <- runSolutionForPart inputLines part2 2 |> Task.await

    Task.ok {}

runSolutionForPart = \input, solution, part ->
    beforePart <- Utc.now |> Task.await
    partAnswer = solution input
    afterPart <- Utc.now |> Task.await
    milliseconds = Utc.deltaAsMillis beforePart afterPart

    Stdout.line "Result for part \(Num.toStr part) in \(Num.toStr milliseconds)ms: \(partAnswer)"

solutionsForDay = \day ->
    when day is
        01 -> Ok (Day01.part1, Day01.part2)
        02 -> Ok (Day02.part1, Day02.part2)
        03 -> Ok (Day03.part1, Day03.part2)
        04 -> Ok (Day04.part1, Day04.part2)
        05 -> Ok (Day05.part1, Day05.part2)
        06 -> Ok (Day06.part1, Day06.part2)
        07 -> Ok (Day07.part1, Day07.part2)
        08 -> Ok (Day08.part1, Day08.part2)
        09 -> Ok (Day09.part1, Day09.part2)
        10 -> Ok (Day10.part1, Day10.part2)
        11 -> Ok (Day11.part1, Day11.part2)
        12 -> Ok (Day12.part1, Day12.part2)
        13 -> Ok (Day13.part1, Day13.part2)
        14 -> Ok (Day14.part1, Day14.part2)
        15 -> Ok (Day15.part1, Day15.part2)
        16 -> Ok (Day16.part1, Day16.part2)
        17 -> Ok (Day17.part1, Day17.part2)
        18 -> Ok (Day18.part1, Day18.part2)
        19 -> Ok (Day19.part1, Day19.part2)
        20 -> Ok (Day20.part1, Day20.part2)
        21 -> Ok (Day21.part1, Day21.part2)
        22 -> Ok (Day22.part1, Day22.part2)
        23 -> Ok (Day23.part1, Day23.part2)
        24 -> Ok (Day24.part1, Day24.part2)
        25 -> Ok (Day25.part1, Day25.part2)
        _ -> Err (DayNotImplemented day)

getDayFromArgs =
    args <- Arg.list
        |> Task.mapErr (\_ -> MissingDayArg)
        |> Task.await

    result =
        args
        |> List.get 1
        |> Result.try Str.toU64

    when result is
        Ok day -> Task.ok day
        Err InvalidNumStr -> Task.err InvalidDayArg
        Err OutOfBounds -> Task.err MissingDayArg

inputCacheDir = ".input"

inputCachePath = \day ->
    Path.fromStr "\(inputCacheDir)/\(Num.toStr day)"

loadInput = \day ->
    result <- loadInputFromCache day |> Task.attempt

    when result is
        Ok lines -> Task.ok lines
        Err InputNotCachedYet ->
            lines <- fetchInputFromInternet day |> Task.await
            _ <- saveInputToCache day lines |> Task.await

            Task.ok lines

fetchInputFromInternet = \day ->
    sessionToken <- loadSessionToken |> Task.await

    url = "https://adventofcode.com/\(Num.toStr aocYear)/day/\(Num.toStr day)/input"
    cookieHeader = Http.header "Cookie" "session=\(sessionToken)"
    request = { Http.defaultRequest & url, headers: [cookieHeader] }
    response = Http.send request
        |> Task.mapErr! FailedToFetchInput

    response.body
    |> Str.fromUtf8
    |> Result.withDefault ""
    |> Str.split "\n"
    |> Task.ok

loadInputFromCache = \day ->
    data <- File.readUtf8 (inputCachePath day)
        |> Task.mapErr (\_ -> InputNotCachedYet)
        |> Task.await

    data |> Str.split "\n" |> Task.ok

loadSessionToken =
    tokenPath = Path.fromStr ".session"

    sessionToken <- File.readUtf8 tokenPath
        |> Task.mapErr (\_ -> MissingSessionToken)
        |> Task.await

    Task.ok (Str.trim sessionToken)

createCacheDirIfMissing =
    dirPath = Path.fromStr inputCacheDir
    result <- Dir.create dirPath |> Task.attempt

    when result is
        Ok _ | Err AlreadyExists -> Task.ok {}
        Err _ -> Task.err FailedToCreateCacheDir

saveInputToCache = \day, lines ->
    cachePath = inputCachePath day
    _ <- createCacheDirIfMissing |> Task.await

    _ <- File.writeUtf8 cachePath (Str.joinWith lines "\n")
        |> Task.mapErr (\_ -> FailedToCacheInput)
        |> Task.await

    Task.ok {}
