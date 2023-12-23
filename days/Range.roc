interface Range
    exposes [
        Range,
        len,
        contains,
        combineAll,
    ]
    imports []

Range a : {
    start : Num a,
    end : Num a,
}

len : Range a -> Num a
len = \range ->
    range.end + 1 - range.start

contains : Range a, Num a -> Bool
contains = \range, value ->
    range.start <= value && range.end >= value

combineAll : List (Range a) -> List (Range a)
combineAll = \ranges ->
    sortedRanges =
        ranges
        |> List.sortWith \a, b -> Num.compare a.start b.start
    List.walk sortedRanges [] \combined, range ->
        when List.first combined is
            Err _ -> [range]
            Ok priorRange ->
                if priorRange.end <= range.start then
                    combined
                    |> List.dropFirst 1
                    |> List.append { start: priorRange.start, end: range.end }
                else
                    combined
                    |> List.append range
