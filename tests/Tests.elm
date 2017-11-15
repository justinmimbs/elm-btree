module Tests exposing (..)

import Char
import Dict.BTree as Dict exposing (Dict(..))
import Expect
import Test exposing (Test, test, describe)


isValidTest : Test
isValidTest =
    describe "isValid"
        [ test "catches duplicate keys" <|
            \() ->
                N2 (N3 Leaf ( 'a', 'a' ) Leaf ( 'b', 'b' ) Leaf) ( 'c', 'c' ) (N3 Leaf ( 'c', 'c' ) Leaf ( 'd', 'd' ) Leaf)
                    |> isValid
                    |> Expect.false "broken invariant not caught"
        , test "catches unsorted keys" <|
            \() ->
                N2 (N3 Leaf ( 'a', 'a' ) Leaf ( 'b', 'b' ) Leaf) ( 'c', 'c' ) (N3 Leaf ( 'e', 'e' ) Leaf ( 'd', 'd' ) Leaf)
                    |> isValid
                    |> Expect.false "broken invariant not caught"
        , test "catches uneven leaves" <|
            \() ->
                N2 (N3 Leaf ( 'a', 'a' ) Leaf ( 'b', 'b' ) Leaf) ( 'c', 'c' ) (N3 Leaf ( 'd', 'd' ) (N2 Leaf ( 'e', 'e' ) Leaf) ( 'f', 'f' ) Leaf)
                    |> isValid
                    |> Expect.false "broken invariant not caught"
        ]


insert : Test
insert =
    describe "insert"
        [ test "builds as expected" <|
            \() ->
                "CSDTAMPIBWNGURKEHOLJYQZFXV"
                    |> stringToPairs
                    |> List.foldl (uncurry Dict.insert) Dict.empty
                    |> Expect.equal
                        (N2
                            (N4
                                (N4 Leaf ( 'A', 'a' ) Leaf ( 'B', 'b' ) Leaf ( 'C', 'c' ) Leaf)
                                ( 'D', 'd' )
                                (N4 Leaf ( 'E', 'e' ) Leaf ( 'F', 'f' ) Leaf ( 'G', 'g' ) Leaf)
                                ( 'H', 'h' )
                                (N3 Leaf ( 'I', 'i' ) Leaf ( 'J', 'j' ) Leaf)
                                ( 'K', 'k' )
                                (N3 Leaf ( 'L', 'l' ) Leaf ( 'M', 'm' ) Leaf)
                            )
                            ( 'N', 'n' )
                            (N4
                                (N3 Leaf ( 'O', 'o' ) Leaf ( 'P', 'p' ) Leaf)
                                ( 'Q', 'q' )
                                (N2 Leaf ( 'R', 'r' ) Leaf)
                                ( 'S', 's' )
                                (N4 Leaf ( 'T', 't' ) Leaf ( 'U', 'u' ) Leaf ( 'V', 'v' ) Leaf)
                                ( 'W', 'w' )
                                (N4 Leaf ( 'X', 'x' ) Leaf ( 'Y', 'y' ) Leaf ( 'Z', 'z' ) Leaf)
                            )
                        )
        , describe "maintains invariants"
            (List.range 1 100
                |> List.map
                    (\n ->
                        test ("at size " ++ toString n) <|
                            \() ->
                                List.range 1 n
                                    |> List.foldl (\i -> Dict.insert i i) Dict.empty
                                    |> isValid
                                    |> Expect.true "broken invariant"
                    )
            )
        ]


fromList : Test
fromList =
    describe "fromList"
        [ describe "maintains invariants"
            (List.range 1 100
                |> List.map
                    (\n ->
                        test ("at size " ++ toString n) <|
                            \() ->
                                List.range 1 n
                                    |> List.map (\i -> ( i, i ))
                                    |> Dict.fromList
                                    |> isValid
                                    |> Expect.true "broken invariant"
                    )
            )
        ]


get : Test
get =
    let
        letters =
            "CSDTAMPIBWNGURKEHOLJYQZFXV"

        dict =
            letters |> stringToPairs |> Dict.fromList
    in
        describe "get"
            [ test "returns Just values for all its keys" <|
                \() ->
                    letters
                        |> String.toList
                        |> List.filterMap (\c -> Dict.get c dict)
                        |> String.fromList
                        |> Expect.equal (letters |> String.toLower)
            , test "returns Nothing for keys it doesn't have" <|
                \() ->
                    "0123456789"
                        |> String.toList
                        |> List.filterMap (\c -> Dict.get c dict)
                        |> Expect.equal []
            ]


remove : Test
remove =
    let
        dict =
            dictRange 1 100
    in
        describe "remove"
            [ describe "maintains invariants"
                (List.range 1 100
                    |> List.map
                        (\n ->
                            test ("at size " ++ toString n) <|
                                \() ->
                                    List.range 1 n
                                        |> List.foldr Dict.remove dict
                                        |> isValid
                                        |> Expect.true "broken invariant"
                        )
                )
            ]


union : Test
union =
    describe "union"
        [ test "combines as expected" <|
            \() ->
                Dict.union
                    ("BRAVO" |> stringToPairs |> Dict.fromList)
                    ("CHARLIE" |> stringToPairs |> Dict.fromList)
                    |> Dict.keys
                    |> Expect.equal ("ABCEHILORV" |> String.toList)
        , test "overlapping ranges" <|
            \() ->
                (Dict.union (dictRange 1 200) (dictRange 100 300) |> Dict.keys)
                    |> Expect.equal (List.range 1 300)
        , test "right empty" <|
            \() ->
                (Dict.union (dictRange 1 10) Dict.empty |> Dict.keys)
                    |> Expect.equal (List.range 1 10)
        , test "left empty" <|
            \() ->
                (Dict.union Dict.empty (dictRange 11 20) |> Dict.keys)
                    |> Expect.equal (List.range 11 20)
        , test "disjoint ranges" <|
            \() ->
                (Dict.union (dictRange 1 10) (dictRange 11 20) |> Dict.keys)
                    |> Expect.equal (List.range 1 20)
        ]


intersect : Test
intersect =
    describe "intersect"
        [ test "combines as expected" <|
            \() ->
                Dict.intersect
                    ("BRAVO" |> stringToPairs |> Dict.fromList)
                    ("CHARLIE" |> stringToPairs |> Dict.fromList)
                    |> Dict.keys
                    |> Expect.equal ("AR" |> String.toList)
        , test "overlapping ranges" <|
            \() ->
                (Dict.intersect (dictRange 1 200) (dictRange 100 300) |> Dict.keys)
                    |> Expect.equal (List.range 100 200)
        , test "right empty" <|
            \() ->
                (Dict.intersect (dictRange 1 10) Dict.empty |> Dict.keys)
                    |> Expect.equal []
        , test "left empty" <|
            \() ->
                (Dict.intersect Dict.empty (dictRange 11 20) |> Dict.keys)
                    |> Expect.equal []
        , test "disjoint ranges" <|
            \() ->
                (Dict.intersect (dictRange 1 10) (dictRange 11 20) |> Dict.keys)
                    |> Expect.equal []
        ]


diff : Test
diff =
    describe "diff"
        [ test "combines as expected" <|
            \() ->
                Dict.diff
                    ("BRAVO" |> stringToPairs |> Dict.fromList)
                    ("CHARLIE" |> stringToPairs |> Dict.fromList)
                    |> Dict.keys
                    |> Expect.equal ("BOV" |> String.toList)
        , test "overlapping ranges" <|
            \() ->
                (Dict.diff (dictRange 1 200) (dictRange 100 300) |> Dict.keys)
                    |> Expect.equal (List.range 1 99)
        , test "right empty" <|
            \() ->
                (Dict.diff (dictRange 1 10) Dict.empty |> Dict.keys)
                    |> Expect.equal (List.range 1 10)
        , test "left empty" <|
            \() ->
                (Dict.diff Dict.empty (dictRange 11 20) |> Dict.keys)
                    |> Expect.equal []
        , test "disjoint ranges" <|
            \() ->
                (Dict.diff (dictRange 1 10) (dictRange 11 20) |> Dict.keys)
                    |> Expect.equal (List.range 1 10)
        ]


merge : Test
merge =
    let
        takeA k _ list =
            k :: list

        takeAB k _ _ list =
            k :: list

        skipA _ _ list =
            list

        skipAB _ _ _ list =
            list
    in
        describe "merge"
            [ test "union" <|
                \() ->
                    (Dict.merge takeA takeAB takeA (dictRange 1 10) (dictRange 6 15) [] |> List.reverse)
                        |> Expect.equal (List.range 1 15)
            , test "empty" <|
                \() ->
                    (Dict.merge skipA skipAB skipA (dictRange 1 10) (dictRange 6 15) [] |> List.reverse)
                        |> Expect.equal []
            , test "intersection" <|
                \() ->
                    (Dict.merge skipA takeAB skipA (dictRange 1 10) (dictRange 6 15) [] |> List.reverse)
                        |> Expect.equal (List.range 6 10)
            , test "symmetric difference" <|
                \() ->
                    (Dict.merge takeA skipAB takeA (dictRange 1 10) (dictRange 6 15) [] |> List.reverse)
                        |> Expect.equal (List.range 1 5 ++ List.range 11 15)
            , test "left complement" <|
                \() ->
                    (Dict.merge skipA skipAB takeA (dictRange 1 10) (dictRange 6 15) [] |> List.reverse)
                        |> Expect.equal (List.range 11 15)
            , test "right complement" <|
                \() ->
                    (Dict.merge takeA skipAB skipA (dictRange 1 10) (dictRange 6 15) [] |> List.reverse)
                        |> Expect.equal (List.range 1 5)
            , test "left" <|
                \() ->
                    (Dict.merge takeA takeAB skipA (dictRange 1 10) (dictRange 6 15) [] |> List.reverse)
                        |> Expect.equal (List.range 1 10)
            , test "right" <|
                \() ->
                    (Dict.merge skipA takeAB takeA (dictRange 1 10) (dictRange 6 15) [] |> List.reverse)
                        |> Expect.equal (List.range 6 15)
            ]



-- helpers


stringToPairs : String -> List ( Char, Char )
stringToPairs =
    String.toList >> List.map (\c -> ( c, Char.toLower c ))


dictRange : Int -> Int -> Dict Int String
dictRange x y =
    List.range x y |> List.map (\n -> ( n, toString n )) |> Dict.fromList



-- invariant checks


isValid : Dict comparable v -> Bool
isValid node =
    areKeysAscending node
        && areLeavesAtSameDepth node


{-| (1) each key is greater than the previous (keys are in ascending order and unique)
-}
areKeysAscending : Dict comparable v -> Bool
areKeysAscending =
    Dict.foldl (\key1 _ ( mKey0, ordered ) -> ( Just key1, ordered && unwrap True ((>) key1) mKey0 )) ( Nothing, True ) >> Tuple.second


{-| (2) all leaves are stored at the same depth
-}
areLeavesAtSameDepth : Dict k v -> Bool
areLeavesAtSameDepth node =
    case node of
        Leaf ->
            True

        N2 a _ b ->
            (depthLeft a == depthLeft b)
                && areLeavesAtSameDepth a
                && areLeavesAtSameDepth b

        N3 a _ b _ c ->
            (List.repeat 2 (depthLeft a) == [ depthLeft b, depthLeft c ])
                && areLeavesAtSameDepth a
                && areLeavesAtSameDepth b
                && areLeavesAtSameDepth c

        N4 a _ b _ c _ d ->
            (List.repeat 3 (depthLeft a) == [ depthLeft b, depthLeft c, depthLeft d ])
                && areLeavesAtSameDepth a
                && areLeavesAtSameDepth b
                && areLeavesAtSameDepth c
                && areLeavesAtSameDepth d


depthLeft : Dict k v -> Int
depthLeft node =
    case node of
        Leaf ->
            0

        N2 a _ _ ->
            1 + depthLeft a

        N3 a _ _ _ _ ->
            1 + depthLeft a

        N4 a _ _ _ _ _ _ ->
            1 + depthLeft a



-- Maybe


unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default f m =
    case m of
        Just x ->
            f x

        Nothing ->
            default
