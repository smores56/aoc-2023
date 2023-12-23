app "aoc-2023"
    packages { days: "days/main.roc", pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.File,
        pf.Http,
        pf.Task,
        pf.Path,
        pf.Dir,
        pf.Arg,
        pf.Utc,
        days.Day01,
        days.Day02,
        days.Day03,
        days.Day04,
        days.Day05,
        days.Day06,
        days.Day07,
        days.Day08,
        days.Day09,
        days.Day10,
        days.Day11,
        days.Day12,
        days.Day13,
        days.Day14,
        days.Day15,
        days.Day16,
        days.Day17,
        days.Day18,
        days.Day20,
        days.Day21,
    ]
    provides [main] to pf

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
                "Failed to fetch input from the Advent of Code site: \(Http.errorToString httpErr)"

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
        20 -> Ok (Day20.part1, Day20.part2)
        21 -> Ok (Day21.part1, Day21.part2)
        _ -> Err (DayNotImplemented day)

getDayFromArgs =
    args <- Arg.list
        |> Task.mapErr (\_ -> MissingDayArg)
        |> Task.await

    result =
        args
        |> List.get 1
        |> Result.try Str.toNat

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
    response <- Http.send request
        |> Task.mapErr FailedToFetchInput
        |> Task.await

    response |> Str.split "\n" |> Task.ok

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
