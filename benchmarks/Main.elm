module Main exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram)
import Dict
import Dict.BTree as Dict2


( dictName, dict2Name ) =
    ( "Dict", "BTree" )


main : BenchmarkProgram
main =
    Benchmark.Runner.program (suite 100)


suite : Int -> Benchmark
suite n =
    Benchmark.describe (toString n ++ " elements")
        [ suiteBuild n
        , suiteQuery n
        , suiteModify n
        , suiteTransform n
        , suiteCombineFragmented n
        , suiteCombineContiguous n
        , suiteCombineDisjoint n
        , suiteCombineEmpty n
        ]


suiteBuild : Int -> Benchmark
suiteBuild n =
    let
        q =
            n // 4

        assocListUnsorted =
            List.map4
                (\a b c d -> [ a, b, c, d ])
                (assocListRange 1 q)
                (assocListRange (q + 1) (2 * q) |> List.reverse)
                (assocListRange (2 * q + 1) (3 * q))
                (assocListRange (3 * q + 1) n |> List.reverse)
                |> List.concat

        assocListSorted =
            assocListRange 1 n

        assocListHalfSorted =
            assocListRange (q + 1) (3 * q) ++ assocListRange 1 q ++ assocListRange (3 * q + 1) n

        assocListAlmostSorted =
            assocListRange 1 (n - 1) ++ [ ( n - 1, 0 ) ]
    in
    Benchmark.describe "Build"
        [ Benchmark.compare "fromList (sorted)"
            dictName
            (\() -> Dict.fromList assocListSorted)
            dict2Name
            (\() -> Dict2.fromList assocListSorted)
        , Benchmark.compare "fromList (almost sorted)"
            dictName
            (\() -> Dict.fromList assocListAlmostSorted)
            dict2Name
            (\() -> Dict2.fromList assocListAlmostSorted)
        , Benchmark.compare "fromList (half sorted)"
            dictName
            (\() -> Dict.fromList assocListHalfSorted)
            dict2Name
            (\() -> Dict2.fromList assocListHalfSorted)
        , Benchmark.compare "fromList (unsorted)"
            dictName
            (\() -> Dict.fromList assocListUnsorted)
            dict2Name
            (\() -> Dict2.fromList assocListUnsorted)
        , Benchmark.compare "insert (from empty)"
            dictName
            (\() -> List.foldl (uncurry Dict.insert) Dict.empty assocListUnsorted)
            dict2Name
            (\() -> List.foldl (uncurry Dict2.insert) Dict2.empty assocListUnsorted)
        ]


suiteQuery : Int -> Benchmark
suiteQuery n =
    let
        assocList =
            assocListRange 1 n

        keys =
            assocList |> List.map Tuple.first

        ( dict, dict2 ) =
            ( Dict.fromList assocList, Dict2.fromList assocList )
    in
    Benchmark.describe "Query"
        [ Benchmark.compare "get"
            dictName
            (\() -> List.map (flip Dict.get dict) keys)
            dict2Name
            (\() -> List.map (flip Dict2.get dict2) keys)
        ]


suiteModify : Int -> Benchmark
suiteModify n =
    let
        -- make dicts from evens; use odds to double their size
        ( evens, odds ) =
            assocListRange 1 (2 * n) |> List.partition (\( k, _ ) -> k % 2 == 0)

        ( keys, oddKeys ) =
            ( evens |> List.map Tuple.first, odds |> List.map Tuple.first )

        ( dict, dict2 ) =
            ( Dict.fromList evens, Dict2.fromList evens )
    in
    Benchmark.describe "Modify"
        [ Benchmark.compare "insert (doubling size)"
            dictName
            (\() -> List.foldl (uncurry Dict.insert) dict odds)
            dict2Name
            (\() -> List.foldl (uncurry Dict2.insert) dict2 odds)
        , Benchmark.compare "remove (until empty)"
            dictName
            (\() -> List.foldl Dict.remove dict keys)
            dict2Name
            (\() -> List.foldl Dict2.remove dict2 keys)
        , Benchmark.compare "remove (each key once)"
            dictName
            (\() -> List.map (flip Dict.remove dict) keys)
            dict2Name
            (\() -> List.map (flip Dict2.remove dict2) keys)
        , Benchmark.compare "update (insert, doubling size)"
            dictName
            (\() -> List.foldl (flip Dict.update (\_ -> Just 0)) dict oddKeys)
            dict2Name
            (\() -> List.foldl (flip Dict2.update (\_ -> Just 0)) dict2 oddKeys)
        , Benchmark.compare "update (remove, until empty)"
            dictName
            (\() -> List.foldl (flip Dict.update (\_ -> Nothing)) dict keys)
            dict2Name
            (\() -> List.foldl (flip Dict2.update (\_ -> Nothing)) dict2 keys)
        , Benchmark.compare "update (replace all values)"
            dictName
            (\() -> List.foldl (flip Dict.update (\_ -> Just 0)) dict keys)
            dict2Name
            (\() -> List.foldl (flip Dict2.update (\_ -> Just 0)) dict2 keys)
        , Benchmark.compare "update (no change)"
            dictName
            (\() -> List.foldl (flip Dict.update (\_ -> Nothing)) dict oddKeys)
            dict2Name
            (\() -> List.foldl (flip Dict2.update (\_ -> Nothing)) dict2 oddKeys)
        ]


suiteTransform : Int -> Benchmark
suiteTransform n =
    let
        assocList =
            assocListRange 1 n

        ( dict, dict2 ) =
            ( Dict.fromList assocList, Dict2.fromList assocList )
    in
    Benchmark.describe "Transform"
        [ Benchmark.compare "filter"
            dictName
            (\() -> Dict.filter (\k _ -> k % 2 == 0) dict)
            dict2Name
            (\() -> Dict2.filter (\k _ -> k % 2 == 0) dict2)
        , Benchmark.compare "partition"
            dictName
            (\() -> Dict.partition (\k _ -> k % 2 == 0) dict)
            dict2Name
            (\() -> Dict2.partition (\k _ -> k % 2 == 0) dict2)
        ]


suiteCombineFragmented : Int -> Benchmark
suiteCombineFragmented n =
    suiteCombine "Combine (fragmented intersections)"
        -- multiples of 2
        (assocListRange 1 (2 * n) |> List.filter (\( k, _ ) -> k % 2 == 0))
        -- multiples of 3
        (assocListRange 1 (3 * n) |> List.filter (\( k, _ ) -> k % 3 == 0))


suiteCombineContiguous : Int -> Benchmark
suiteCombineContiguous n =
    suiteCombine "Combine (contiguous intersection)"
        (assocListRange 1 n)
        (assocListRange (n // 2 + 1) (n // 2 + n))


suiteCombineDisjoint : Int -> Benchmark
suiteCombineDisjoint n =
    suiteCombine "Combine (disjoint ranges)"
        (assocListRange 1 n)
        (assocListRange (n + 1) (n + n))


suiteCombineEmpty : Int -> Benchmark
suiteCombineEmpty n =
    suiteCombine "Combine (with one empty)"
        (assocListRange 1 n)
        []


suiteCombine : String -> List ( comparable, v ) -> List ( comparable, v ) -> Benchmark
suiteCombine description leftList rightList =
    let
        ( left, right ) =
            ( Dict.fromList leftList, Dict.fromList rightList )

        ( left2, right2 ) =
            ( Dict2.fromList leftList, Dict2.fromList rightList )
    in
    Benchmark.describe description
        [ Benchmark.compare "union"
            dictName
            (commutative Dict.union left right)
            dict2Name
            (commutative Dict2.union left2 right2)
        , Benchmark.compare "intersect"
            dictName
            (commutative Dict.intersect left right)
            dict2Name
            (commutative Dict2.intersect left2 right2)
        , Benchmark.compare "diff"
            dictName
            (commutative Dict.diff left right)
            dict2Name
            (commutative Dict2.diff left2 right2)
        ]


assocListRange : Int -> Int -> List ( Int, Int )
assocListRange start end =
    List.range start end |> List.map (\x -> ( x, x ))


commutative : (a -> a -> b) -> a -> a -> (() -> ( b, b ))
commutative op left right =
    \() -> ( op left right, op right left )
