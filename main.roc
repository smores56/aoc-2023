app "aoc-2023"
    packages { days: "days/main.roc", pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br" }
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
        days.Day1,
        days.Day2,
        days.Day3,
    ]
    provides [main] to pf

aocYear = 2023

main =
    result <- Task.attempt findSolutionsForDay

    when result is
        Ok {} -> Task.ok {}
        Err err ->
            message =
                when err is
                    DayNotImplemented day -> "Day \(Num.toStr day) has not been implemented yet"
                    InvalidDayArg -> "The day argument must be a positive integer"
                    MissingDayArg -> "You must pass a day of the month to run solutions for"
                    MissingSessionToken -> "You have not saved your session token to .session, check the README for directions"
                    FailedToCacheInput -> "Failed to save loaded input to cache"
                    FailedToCreateCacheDir -> "Failed to create \(inputCacheDir) "
                    FailedToFetchInput httpErr ->
                        "Failed to fetch input from the Advent of Code site: \(Http.errorToString httpErr)"

            Stderr.line message

findSolutionsForDay =
    day <- getDayFromArgs |> Task.await
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

solutionsForDay = \day ->
    when day is
        1 -> Ok (Day1.part1, Day1.part2)
        2 -> Ok (Day2.part1, Day2.part2)
        3 -> Ok (Day3.part1, Day3.part2)
        _ -> Err (DayNotImplemented day)

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
