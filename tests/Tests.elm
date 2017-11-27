module Tests exposing (..)

import Char
import Dict.BTree as Dict exposing (Dict(..))
import Expect
import Test exposing (Test, describe, test)


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
        [ test "inserts key-value pairs" <|
            \() ->
                ("CSDTAMPIBWNGURKEHOLJYQZFXV" |> stringToPairs |> List.foldl (uncurry Dict.insert) Dict.empty)
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
        , test "replaces values for existing keys" <|
            \() ->
                Dict.empty
                    |> Dict.insert 1 'A'
                    |> Dict.insert 1 'B'
                    |> Expect.equal (Dict.insert 1 'B' Dict.empty)
        , describe "maintains invariants"
            (List.range 1 100
                |> List.map
                    (\n ->
                        test ("at size " ++ toString n) <|
                            \() ->
                                (List.range 1 n |> List.foldl (\i -> Dict.insert i i) Dict.empty)
                                    |> isValid
                                    |> Expect.true "broken invariant"
                    )
            )
        ]


foldl : Test
foldl =
    describe "foldl"
        [ test "visits pairs in ascending order" <|
            \() ->
                ("CSDTAMPIBWNGURKEHOLJYQZFXV" |> stringToPairs |> List.foldl (uncurry Dict.insert) Dict.empty)
                    |> Dict.foldl (\k v -> (::) ( k, v )) []
                    |> Expect.equal ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> stringToPairs |> List.reverse)
        ]


foldr : Test
foldr =
    describe "foldr"
        [ test "visits pairs in descending order" <|
            \() ->
                ("CSDTAMPIBWNGURKEHOLJYQZFXV" |> stringToPairs |> List.foldl (uncurry Dict.insert) Dict.empty)
                    |> Dict.foldr (\k v -> (::) ( k, v )) []
                    |> Expect.equal ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> stringToPairs)
        ]


keys : Test
keys =
    describe "keys"
        [ test "returns a sorted list of keys" <|
            \() ->
                ("CSDTAMPIBWNGURKEHOLJYQZFXV" |> stringToPairs |> List.foldl (uncurry Dict.insert) Dict.empty)
                    |> Dict.keys
                    |> Expect.equal ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> String.toList)
        ]


values : Test
values =
    describe "values"
        [ test "returns a sorted list of values" <|
            \() ->
                ("CSDTAMPIBWNGURKEHOLJYQZFXV" |> stringToPairs |> List.foldl (uncurry Dict.insert) Dict.empty)
                    |> Dict.values
                    |> Expect.equal ("abcdefghijklmnopqrstuvwxyz" |> String.toList)
        ]


toList : Test
toList =
    describe "toList"
        [ test "returns a sorted list of pairs" <|
            \() ->
                ("CSDTAMPIBWNGURKEHOLJYQZFXV" |> stringToPairs |> List.foldl (uncurry Dict.insert) Dict.empty)
                    |> Dict.toList
                    |> Expect.equal ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> stringToPairs)
        ]


fromList : Test
fromList =
    describe "fromList"
        [ test "builds equivalently to folding over a list and inserting pairs" <|
            \() ->
                Expect.equal
                    ("CSDTAMPIBWNGURKEHOLJYQZFXV" |> stringToPairs |> List.foldl (uncurry Dict.insert) Dict.empty |> Dict.toList)
                    ("CSDTAMPIBWNGURKEHOLJYQZFXV" |> stringToPairs |> Dict.fromList |> Dict.toList)
        , test "doesn't insert duplicate keys" <|
            \() ->
                ([ ( 1, 'A' ), ( 2, 'B' ), ( 1, 'A' ), ( 2, 'B' ) ] |> Dict.fromList)
                    |> Expect.equal ([ ( 1, 'A' ), ( 2, 'B' ) ] |> Dict.fromList)
        , test "uses the last value among multiple pairs having the same key" <|
            \() ->
                ([ ( 1, 'A' ), ( 1, 'B' ), ( 1, 'C' ), ( 1, 'D' ), ( 1, 'E' ), ( 1, 'F' ) ] |> Dict.fromList)
                    |> Expect.equal ([ ( 1, 'F' ) ] |> Dict.fromList)
        , describe "maintains invariants"
            (List.range 1 100
                |> List.map
                    (\n ->
                        test ("at size " ++ toString n) <|
                            \() ->
                                (List.range 1 n |> List.map (\i -> ( i, i )) |> Dict.fromList)
                                    |> isValid
                                    |> Expect.true "broken invariant"
                    )
            )
        ]


size : Test
size =
    describe "size"
        [ test "returns the number of pairs in a dict" <|
            \() ->
                ("CSDTAMPIBWNGURKEHOLJYQZFXV" |> stringToPairs |> List.foldl (uncurry Dict.insert) Dict.empty)
                    |> Dict.size
                    |> Expect.equal 26
        , test "empty" <|
            \() ->
                Dict.size Dict.empty |> Expect.equal 0
        , test "singleton" <|
            \() ->
                Dict.size (Dict.singleton "" "") |> Expect.equal 1
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
        [ test "removes a pair by key" <|
            \() ->
                ([ ( 1, 'A' ), ( 2, 'B' ) ] |> Dict.fromList)
                    |> Dict.remove 1
                    |> Expect.equal ([ ( 2, 'B' ) ] |> Dict.fromList)
        , test "doesn't alter the dict when key isn't found" <|
            \() ->
                ([ ( 1, 'A' ), ( 2, 'B' ) ] |> Dict.fromList)
                    |> Dict.remove 3
                    |> Expect.equal ([ ( 1, 'A' ), ( 2, 'B' ) ] |> Dict.fromList)
        , describe "maintains invariants"
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


update : Test
update =
    describe "update"
        [ test "can insert" <|
            \() ->
                ([ ( 1, 'A' ), ( 2, 'B' ) ] |> Dict.fromList)
                    |> Dict.update 3 (\_ -> Just 'C')
                    |> Expect.equal ([ ( 1, 'A' ), ( 2, 'B' ), ( 3, 'C' ) ] |> Dict.fromList)
        , test "can replace values" <|
            \() ->
                ([ ( 1, 'A' ), ( 2, 'B' ) ] |> Dict.fromList)
                    |> Dict.update 2 (\_ -> Just 'C')
                    |> Expect.equal ([ ( 1, 'A' ), ( 2, 'C' ) ] |> Dict.fromList)
        , test "can remove" <|
            \() ->
                ([ ( 1, 'A' ), ( 2, 'B' ) ] |> Dict.fromList)
                    |> Dict.update 2 (\_ -> Nothing)
                    |> Expect.equal ([ ( 1, 'A' ) ] |> Dict.fromList)
        , test "may do nothing" <|
            \() ->
                ([ ( 1, 'A' ), ( 2, 'B' ) ] |> Dict.fromList)
                    |> Dict.update 3 (\_ -> Nothing)
                    |> Expect.equal ([ ( 1, 'A' ), ( 2, 'B' ) ] |> Dict.fromList)
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
        take k _ list =
            k :: list

        take_ k _ _ list =
            k :: list

        skip _ _ list =
            list

        skip_ _ _ _ list =
            list
    in
    describe "merge"
        [ test "union" <|
            \() ->
                (Dict.merge take take_ take (dictRange 1 10) (dictRange 6 15) [] |> List.reverse)
                    |> Expect.equal (List.range 1 15)
        , test "empty" <|
            \() ->
                (Dict.merge skip skip_ skip (dictRange 1 10) (dictRange 6 15) [] |> List.reverse)
                    |> Expect.equal []
        , test "intersection" <|
            \() ->
                (Dict.merge skip take_ skip (dictRange 1 10) (dictRange 6 15) [] |> List.reverse)
                    |> Expect.equal (List.range 6 10)
        , test "symmetric difference" <|
            \() ->
                (Dict.merge take skip_ take (dictRange 1 10) (dictRange 6 15) [] |> List.reverse)
                    |> Expect.equal (List.range 1 5 ++ List.range 11 15)
        , test "left complement" <|
            \() ->
                (Dict.merge skip skip_ take (dictRange 1 10) (dictRange 6 15) [] |> List.reverse)
                    |> Expect.equal (List.range 11 15)
        , test "right complement" <|
            \() ->
                (Dict.merge take skip_ skip (dictRange 1 10) (dictRange 6 15) [] |> List.reverse)
                    |> Expect.equal (List.range 1 5)
        , test "left" <|
            \() ->
                (Dict.merge take take_ skip (dictRange 1 10) (dictRange 6 15) [] |> List.reverse)
                    |> Expect.equal (List.range 1 10)
        , test "right" <|
            \() ->
                (Dict.merge skip take_ take (dictRange 1 10) (dictRange 6 15) [] |> List.reverse)
                    |> Expect.equal (List.range 6 15)
        ]


filter : Test
filter =
    let
        isEven n =
            n % 2 == 0
    in
    describe "filter"
        [ test "removes as expected" <|
            \() ->
                (Dict.filter (isEven >> always) (dictRange 1 100) |> Dict.keys)
                    |> Expect.equal (List.range 1 100 |> List.filter isEven)
        , test "may do nothing" <|
            \() ->
                (Dict.filter (always True >> always) (dictRange 1 100) |> Dict.keys)
                    |> Expect.equal (List.range 1 100)
        ]


partition : Test
partition =
    let
        isEven n =
            n % 2 == 0
    in
    describe "partition"
        [ test "partitions as expected" <|
            \() ->
                (Dict.partition (isEven >> always) (dictRange 1 100) |> (\( a, b ) -> ( Dict.keys a, Dict.keys b )))
                    |> Expect.equal (List.range 1 100 |> List.partition isEven)
        , test "all left" <|
            \() ->
                (Dict.partition (always True >> always) (dictRange 1 100) |> (\( a, b ) -> ( Dict.keys a, Dict.keys b )))
                    |> Expect.equal ( List.range 1 100, [] )
        , test "all right" <|
            \() ->
                (Dict.partition (always False >> always) (dictRange 1 100) |> (\( a, b ) -> ( Dict.keys a, Dict.keys b )))
                    |> Expect.equal ( [], List.range 1 100 )
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
