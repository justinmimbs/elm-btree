module Example4 exposing (..)

import Char
import Dict.BTree4 as Dict2 exposing (Node(..))


letters =
    "CSDTAMPIBWNGURKEHOLJYQZFXV"


list =
    String.toList letters
        |> List.map (\c -> ( c, Char.toLower c ))


ex1 =
    Dict2.fromList list


getValuesFromEx1 =
    String.toList ("0123456789" ++ letters ++ " ")
        |> List.filterMap (\c -> Dict2.get c ex1)
        |> String.fromList


tests =
    [ getValuesFromEx1 == String.toLower letters
    , Dict2.toList ex1 == List.sortBy Tuple.first list
    , not (areKeysOrdered duplicates)
    , not (areKeysOrdered unordered)
    , not (areLeavesAtSameDepth uneven)
    , isValid ex1
    ]



-- invalid examples


duplicates =
    K1 (K2 Leaf ( 'a', 'a' ) Leaf ( 'b', 'b' ) Leaf) ( 'c', 'c' ) (K2 Leaf ( 'c', 'c' ) Leaf ( 'd', 'd' ) Leaf)


unordered =
    K1 (K2 Leaf ( 'a', 'a' ) Leaf ( 'b', 'b' ) Leaf) ( 'c', 'c' ) (K2 Leaf ( 'e', 'e' ) Leaf ( 'd', 'd' ) Leaf)


uneven =
    K1 (K2 Leaf ( 'a', 'a' ) Leaf ( 'b', 'b' ) Leaf) ( 'c', 'c' ) (K2 Leaf ( 'd', 'd' ) (K1 Leaf ( 'e', 'e' ) Leaf) ( 'f', 'f' ) Leaf)



-- invariant checks


isValid : Node comparable v -> Bool
isValid node =
    areKeysOrdered node
        && areLeavesAtSameDepth node


{-| (1) each key is greater than the previous (keys are in ascending order and unique)
-}
areKeysOrdered : Node comparable v -> Bool
areKeysOrdered =
    Dict2.foldl (\key1 _ ( mKey0, ordered ) -> ( Just key1, ordered && unwrap True ((>) key1) mKey0 )) ( Nothing, True ) >> Tuple.second


{-| (2) all leaves are stored at the same depth
-}
areLeavesAtSameDepth : Node k v -> Bool
areLeavesAtSameDepth node =
    case node of
        Leaf ->
            True

        K1 a _ b ->
            (depthLeft a == depthLeft b)
                && areLeavesAtSameDepth a
                && areLeavesAtSameDepth b

        K2 a _ b _ c ->
            (List.repeat 2 (depthLeft a) == [ depthLeft b, depthLeft c ])
                && areLeavesAtSameDepth a
                && areLeavesAtSameDepth b
                && areLeavesAtSameDepth c

        K3 a _ b _ c _ d ->
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

        K1 a _ _ ->
            1 + depthLeft a

        K2 a _ _ _ _ ->
            1 + depthLeft a

        K3 a _ _ _ _ _ _ ->
            1 + depthLeft a



-- helpers


unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default f m =
    case m of
        Just x ->
            f x

        Nothing ->
            default
