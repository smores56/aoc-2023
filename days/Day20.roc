interface Day20
    exposes [part1, part2]
    imports [Utils]

parseModule = \line ->
    { before, after } <- Str.splitFirst line " -> "
        |> Result.map
    destinations = Str.split after ", "

    (name, moduleType) =
        if before == "broadcaster" then
            ("broadcaster", Broadcaster)
        else
            (
                Utils.dropGraphemes before 1,
                if Str.startsWith before "%" then
                    FlipFlop Off
                else
                    Conjunction (Dict.empty {}),
            )

    (name, { type: moduleType, destinations })

parseModules = \lines ->
    nonEmptyLines = List.dropIf lines Str.isEmpty
    moduleMap =
        when List.mapTry nonEmptyLines parseModule is
            Ok modules -> Dict.fromList modules
            Err _ -> crash "Assumed modules were well-formed"

    inputsPerModule =
        moduleMap
        |> Dict.walk (Dict.empty {}) \map, name, module ->
            List.walk module.destinations map \map2, destination ->
                inputList =
                    Dict.get map2 destination
                    |> Result.withDefault []
                    |> List.append name
                Dict.insert map2 destination inputList

    Dict.map moduleMap \name, module ->
        when module.type is
            Conjunction _defaultInput ->
                inputs =
                    Dict.get inputsPerModule name
                    |> Result.withDefault []
                    |> List.map \input -> (input, Low)
                    |> Dict.fromList
                { module & type: Conjunction inputs }

            _other -> module

sendSignalToModule = \module, signalSource, sentSignal ->
    when module.type is
        Broadcaster ->
            (module, Ok sentSignal)

        FlipFlop powered ->
            when sentSignal is
                High -> (module, Err NoSignalToSend)
                Low ->
                    (nextPowered, signal) =
                        when powered is
                            On -> (Off, Low)
                            Off -> (On, High)

                    ({ module & type: FlipFlop nextPowered }, Ok signal)

        Conjunction inputHistory ->
            updatedHistory = Dict.insert inputHistory signalSource sentSignal
            allHighPulses =
                Dict.toList updatedHistory
                |> List.all \(_name, lastSignal) -> lastSignal == High

            (
                { module & type: Conjunction updatedHistory },
                Ok (if allHighPulses then Low else High),
            )

sendButtonSignalToModules = \modules ->
    sendSignalToModules modules [("button", "broadcaster", Low)] { low: 0, high: 0 }

sendSignalToModules = \modules, signalQueue, signalCounts ->
    when List.first signalQueue is
        Err _ -> (modules, signalCounts)
        Ok (signalSource, signalDestination, signal) ->
            (updatedModules, signalQueueAdditions) =
                when Dict.get modules signalDestination is
                    Err _ -> (modules, [])
                    Ok module ->
                        (updatedModule, outputSignal) = sendSignalToModule module signalSource signal
                        modulesWithUpdate = Dict.insert modules signalDestination updatedModule
                        queueAdditions =
                            when outputSignal is
                                Err _ -> []
                                Ok output ->
                                    module.destinations
                                    |> List.map \d -> (signalDestination, d, output)

                        (modulesWithUpdate, queueAdditions)

            updatedSignalQueue =
                signalQueue
                |> List.dropFirst 1
                |> List.concat signalQueueAdditions
            updatedCounts =
                when signal is
                    Low -> { signalCounts & low: signalCounts.low + 1 }
                    High -> { signalCounts & high: signalCounts.high + 1 }

            sendSignalToModules updatedModules updatedSignalQueue updatedCounts

buttonPressesUntilRxLow = \modules, signalQueue, pressCount ->
    (signalSource, signalDestination, signal) =
        List.first signalQueue
        |> Result.withDefault ("button", "broadcaster", Low)

    if signalDestination == "rx" && signal == Low then
        pressCount
    else
        (updatedModules, signalQueueAdditions) =
            when Dict.get modules signalDestination is
                Err _ -> (modules, [])
                Ok module ->
                    (updatedModule, outputSignal) = sendSignalToModule module signalSource signal
                    modulesWithUpdate = Dict.insert modules signalDestination updatedModule
                    queueAdditions =
                        when outputSignal is
                            Err _ -> []
                            Ok output ->
                                module.destinations
                                |> List.map \d -> (signalDestination, d, output)

                    (modulesWithUpdate, queueAdditions)

        updatedSignalQueue =
            signalQueue
            |> List.dropFirst 1
            |> List.concat signalQueueAdditions
        newPressCount =
            if signalSource == "button" then
                pressCount + 1
            else
                pressCount

        buttonPressesUntilRxLow updatedModules updatedSignalQueue newPressCount

part1 = \lines ->
    modules = parseModules lines
    buttonPushCount = 1000

    startingCounts = { low: 0, high: 0 }
    (_finalModules, totalCounts) =
        List.range { start: At 0, end: Before buttonPushCount }
        |> List.walk (modules, startingCounts) \(modulesSoFar, countsSoFar), _ ->
            (updatedModules, counts) = sendButtonSignalToModules modulesSoFar
            (updatedModules, { low: counts.low + countsSoFar.low, high: counts.high + countsSoFar.high })

    Num.toStr (totalCounts.low * totalCounts.high)

part2 = \lines ->
    modules = parseModules lines

    buttonPressesUntilRxLow modules [] 0
    |> Num.toStr
