module [
    MinHeap,
    withComparator,
    fromList,
    insert,
    removeMin,
]

MinHeap a := {
    items : List a,
    comparator : a, a -> [LT, EQ, GT],
}

withComparator : (a, a -> [LT, EQ, GT]) -> MinHeap a
withComparator = \comparator ->
    @MinHeap { items: [], comparator }

fromList : List a, (a, a -> [LT, EQ, GT]) -> MinHeap a
fromList = \items, comparator ->
    len = List.len items
    minHeap = @MinHeap { items, comparator }

    List.range { start: At 0, end: Before (len // 2) }
    |> List.reverse
    |> List.walk minHeap \minHeapSoFar, index ->
        heapify minHeapSoFar index

insert : MinHeap a, a -> MinHeap a
insert = \@MinHeap { items, comparator }, item ->
    lastIndex = List.len items
    updatedItems =
        items
        |> List.append item
        |> insertSwapper comparator lastIndex

    @MinHeap { items: updatedItems, comparator }

insertSwapper = \items, comparator, index ->
    if index == 0 then
        items
    else
        parentIndex = (index - 1) // 2
        when (List.get items parentIndex, List.get items index) is
            (Ok parent, Ok item) if comparator parent item == GT ->
                swappedItems = List.swap items parentIndex index
                insertSwapper swappedItems comparator parentIndex

            _ -> items

removeMin : MinHeap a -> Result (a, MinHeap a) [HeapWasEmpty]
removeMin = \@MinHeap { items, comparator } ->
    when List.first items is
        Err ListWasEmpty -> Err HeapWasEmpty
        Ok item ->
            lastIndex = List.len items - 1
            restOfItems =
                items
                |> List.swap 0 lastIndex
                |> List.dropLast 1
            updatedHeap =
                @MinHeap { items: restOfItems, comparator }
                |> heapify 0

            Ok (item, updatedHeap)

heapify : MinHeap a, U64 -> MinHeap a
heapify = \@MinHeap { items, comparator }, index ->
    leftChildIndex = 2 * index + 1
    rightChildIndex = 2 * index + 2

    intermediateSmallestIndex =
        when (List.get items index, List.get items leftChildIndex) is
            (Ok item, Ok leftChild) ->
                if comparator item leftChild == LT then
                    index
                else
                    leftChildIndex

            _ ->
                index

    smallestIndex =
        when (List.get items intermediateSmallestIndex, List.get items rightChildIndex) is
            (Ok intermediateItem, Ok rightChild) ->
                if comparator intermediateItem rightChild == LT then
                    intermediateSmallestIndex
                else
                    rightChildIndex

            _ ->
                intermediateSmallestIndex

    if smallestIndex == index then
        @MinHeap { items, comparator }
    else
        swappedItems = List.swap items index smallestIndex
        @MinHeap { items: swappedItems, comparator }
        |> heapify smallestIndex

expect
    minHeapItems =
        fromList [5, 4, 3, 2, 1] Num.compare
        |> \@MinHeap { items } -> items

    minHeapItems == [1, 2, 3, 5, 4]

expect
    minHeap =
        fromList [5, 4, 3, 2, 1] Num.compare
    removalResult =
        removeMin minHeap
        |> Result.map \(min, @MinHeap { items }) -> (min, List.len items)

    removalResult == Ok (1, 4)

