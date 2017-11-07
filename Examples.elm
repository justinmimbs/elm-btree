module Examples exposing (..)

import Char
import Dict.BTree as Dict exposing (Node(..))


letters =
    "CSDTAMPIBWNGURKEHOLJYQZFXV"


list =
    String.toList letters
        |> List.map (\c -> ( c, Char.toLower c ))


ex1 =
    Dict.fromList list


ex2 =
    List.range -100 100
        |> List.map (\n -> ( n, toString n ))
        |> Dict.fromList


ex3 =
    List.foldl
        Dict.remove
        ex2
        [ -34, 9, -89, -100, 100, 27, -3, 45, 46, 47, 48 ]


getValuesFromEx1 =
    String.toList ("0123456789" ++ letters ++ " ")
        |> List.filterMap (\c -> Dict.get c ex1)
        |> String.fromList


tests =
    [ getValuesFromEx1 == String.toLower letters
    , Dict.toList ex1 == List.sortBy Tuple.first list
    , not (areKeysOrdered duplicates)
    , not (areKeysOrdered unordered)
    , not (areLeavesAtSameDepth uneven)
    , isValid ex1
    , isValid ex2
    , isValid ex3
    , Dict.size ex2 == 201
    , Dict.size ex3 == 190
    , Dict.get 27 ex2 == Just "27"
    , Dict.get 27 ex3 == Nothing
    ]



-- invalid examples


duplicates =
    N2 (N3 Leaf ( 'a', 'a' ) Leaf ( 'b', 'b' ) Leaf) ( 'c', 'c' ) (N3 Leaf ( 'c', 'c' ) Leaf ( 'd', 'd' ) Leaf)


unordered =
    N2 (N3 Leaf ( 'a', 'a' ) Leaf ( 'b', 'b' ) Leaf) ( 'c', 'c' ) (N3 Leaf ( 'e', 'e' ) Leaf ( 'd', 'd' ) Leaf)


uneven =
    N2 (N3 Leaf ( 'a', 'a' ) Leaf ( 'b', 'b' ) Leaf) ( 'c', 'c' ) (N3 Leaf ( 'd', 'd' ) (N2 Leaf ( 'e', 'e' ) Leaf) ( 'f', 'f' ) Leaf)



-- invariant checks


isValid : Node comparable v -> Bool
isValid node =
    areKeysOrdered node
        && areLeavesAtSameDepth node


{-| (1) each key is greater than the previous (keys are in ascending order and unique)
-}
areKeysOrdered : Node comparable v -> Bool
areKeysOrdered =
    Dict.foldl (\key1 _ ( mKey0, ordered ) -> ( Just key1, ordered && unwrap True ((>) key1) mKey0 )) ( Nothing, True ) >> Tuple.second


{-| (2) all leaves are stored at the same depth
-}
areLeavesAtSameDepth : Node k v -> Bool
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


depthLeft : Node k v -> Int
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



-- helpers


unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default f m =
    case m of
        Just x ->
            f x

        Nothing ->
            default
