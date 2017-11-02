module Example exposing (..)

import Char
import Dict.BTree as Dict2 exposing (Node(..))


ex1 =
    String.toList "CSDTAMPIBWNGURKEHOLJYQZFXV"
        |> List.map (\c -> ( c, Char.toLower c ))
        |> Dict2.fromList


ex2 =
    List.range -100 100
        |> List.map (\n -> ( n, n ))
        |> Dict2.fromList


ex3 =
    List.foldl
        Dict2.remove
        ex2
        [ -34, 9, -89, -100, 100, 27, -3, 45, 46, 47, 48 ]



-- invariant checks


isValid : Node comparable v -> Bool
isValid node =
    areKeysOrdered node
        && areLeavesAtSameDepth node
        && areNodeSizesInRange node


{-| (1) each key is greater than the previous (keys are ordered and unique)
-}
areKeysOrdered : Node comparable v -> Bool
areKeysOrdered =
    Dict2.foldl (\key1 _ ( mKey0, ordered ) -> ( Just key1, ordered && unwrap True ((>) key1) mKey0 )) ( Nothing, True ) >> Tuple.second


{-| (2) all leaves are stored at the same depth
-}
areLeavesAtSameDepth : Node k v -> Bool
areLeavesAtSameDepth node =
    case node of
        Leaf _ ->
            True

        NonLeaf n kn ->
            let
                d =
                    depthLeft n
            in
                kn |> List.all (\( _, _, n2 ) -> depthLeft n2 == d && areLeavesAtSameDepth n2)


depthLeft : Node k v -> Int
depthLeft node =
    case node of
        Leaf _ ->
            1

        NonLeaf n _ ->
            1 + depthLeft n


{-| (3) all nodes contain a number of keys within min/max range,
-}
areNodeSizesInRange : Node k v -> Bool
areNodeSizesInRange =
    areNodeSizesInRangeHelp True


areNodeSizesInRangeHelp : Bool -> Node k v -> Bool
areNodeSizesInRangeHelp isRoot node =
    case node of
        Leaf list ->
            (isRoot || Dict2.minKeys <= List.length list) && List.length list <= Dict2.maxKeys

        NonLeaf n kn ->
            ((isRoot || Dict2.minKeys <= List.length kn) && List.length kn <= Dict2.maxKeys)
                && areNodeSizesInRangeHelp False n
                && (kn |> List.all (\( _, _, n2 ) -> areNodeSizesInRangeHelp False n2))



-- invalid examples


duplicates =
    NonLeaf (Leaf [ ( 'a', 'a' ), ( 'c', 'c' ) ]) [ ( 'c', 'c', Leaf [ ( 'd', 'd' ), ( 'e', 'e' ) ] ) ]


unordered =
    NonLeaf (Leaf [ ( 'a', 'a' ), ( 'b', 'b' ) ]) [ ( 'c', 'c', Leaf [ ( 'e', 'e' ), ( 'd', 'd' ) ] ) ]


uneven =
    NonLeaf (Leaf [ ( 'a', 'a' ), ( 'b', 'b' ) ]) [ ( 'c', 'c', Leaf [ ( 'd', 'd' ), ( 'e', 'e' ) ] ), ( 'f', 'f', NonLeaf (Leaf [ ( 'g', 'g' ), ( 'h', 'h' ) ]) [ ( 'i', 'i', Leaf [ ( 'j', 'j' ), ( 'k', 'k' ) ] ) ] ) ]


overfull5 =
    NonLeaf (Leaf [ ( 'a', 'a' ), ( 'b', 'b' ) ]) [ ( 'c', 'c', Leaf (List.repeat 5 ( 'e', 'e' )) ) ]


underfull5 =
    NonLeaf (Leaf [ ( 'a', 'a' ), ( 'b', 'b' ) ]) [ ( 'c', 'c', Leaf [ ( 'e', 'e' ) ] ) ]



-- tests


pass : Bool
pass =
    [ isValid ex1
    , isValid ex2
    , isValid ex3
    , Dict2.size ex1 == 26
    , Dict2.size ex2 == 201
    , Dict2.size ex3 == 190
    , (ex1 |> Dict2.toList |> List.map Tuple.first |> String.fromList) == "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    , (ex2 |> Dict2.toList |> List.map Tuple.first) == List.range -100 100
    , (ex1 |> Dict2.get 'N') == Just 'n'
    , (ex2 |> Dict2.get 45) == Just 45
    , not (areKeysOrdered duplicates)
    , not (areKeysOrdered unordered)
    , not (areLeavesAtSameDepth uneven)
    , not (areNodeSizesInRange overfull5)
    , not (areNodeSizesInRange underfull5)

    -- order-5 only
    , rem1 == rem1_exp
    , rem2 == rem2_exp
    ]
        |> List.all identity



-- `remove` test case examples for an order-5 tree (i.e. maxKeys == 4)


rem1 =
    NonLeaf (Leaf [ ( 'a', 'a' ), ( 'b', 'b' ), ( 'c', 'c' ) ]) [ ( 'd', 'd', Leaf [ ( 'e', 'e' ), ( 'f', 'f' ) ] ), ( 'g', 'g', Leaf [ ( 'h', 'h' ), ( 'i', 'i' ) ] ) ]
        |> Dict2.remove 'f'


rem1_exp =
    NonLeaf (Leaf [ ( 'a', 'a' ), ( 'b', 'b' ) ]) [ ( 'c', 'c', Leaf [ ( 'd', 'd' ), ( 'e', 'e' ) ] ), ( 'g', 'g', Leaf [ ( 'h', 'h' ), ( 'i', 'i' ) ] ) ]


rem2 =
    NonLeaf (Leaf [ ( 'a', 'a' ), ( 'b', 'b' ) ]) [ ( 'c', 'c', Leaf [ ( 'd', 'd' ), ( 'e', 'e' ) ] ), ( 'g', 'g', Leaf [ ( 'h', 'h' ), ( 'i', 'i' ) ] ) ]
        |> Dict2.remove 'd'


rem2_exp =
    NonLeaf (Leaf [ ( 'a', 'a' ), ( 'b', 'b' ), ( 'c', 'c' ), ( 'e', 'e' ) ]) [ ( 'g', 'g', Leaf [ ( 'h', 'h' ), ( 'i', 'i' ) ] ) ]



-- helpers


unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default f m =
    case m of
        Just x ->
            f x

        Nothing ->
            default
