module [searchDistance]

import MinHeap

searchDistance = \params ->
    firstCell = { coords: params.start.coords, value: params.start.value, g: 0, h: 0, f: 0 }
    minFValue = \cellA, cellB ->
        Num.compare cellA.f cellB.f
    initialQueue = MinHeap.fromList [firstCell] minFValue

    innerSearchDistance params initialQueue (Set.empty {})

innerSearchDistance = \params, queue, visited ->
    when MinHeap.removeMin queue is
        Err HeapWasEmpty -> Err NoPathFound
        Ok (current, _restOfQueue) if current.coords == params.goal -> Ok current.g
        Ok (current, restOfQueue) ->
            nextNeighbors =
                params.getNeighbors params.grid { coords: current.coords, value: current.value }
                |> List.dropIf \neighbor ->
                    Set.contains visited neighbor.coords
                |> List.map \neighbor ->
                    neighborG = current.g + neighbor.distance
                    neighborH = params.heuristic {
                        grid: params.grid,
                        goal: params.goal,
                        cell: {
                            value: neighbor.value,
                            coords: neighbor.coords,
                        },
                    }

                    {
                        value: neighbor.value,
                        coords: neighbor.coords,
                        g: neighborG,
                        h: neighborH,
                        f: neighborG + neighborH,
                    }

            updatedVisited = Set.insert visited current.coords
            updatedQueue =
                List.walk nextNeighbors restOfQueue \buildingQueue, neighbor ->
                    MinHeap.insert buildingQueue neighbor

            innerSearchDistance params updatedQueue updatedVisited
