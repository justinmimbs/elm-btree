module Dict.BTree
    exposing
        ( empty
        , get
        , insert
        , remove
        , foldl
        , fromList
        , toList
        , size
          -- for validation only
        , maxKeys
        , minKeys
        , Node(..)
        )


type Node k v
    = Leaf (List ( k, v ))
    | NonLeaf (Node k v) (List ( k, v, Node k v ))


{-| maxKeys == maxNodes - 1
-}
maxKeys : Int
maxKeys =
    4


minKeys : Int
minKeys =
    maxKeys // 2


empty : Node comparable v
empty =
    Leaf []


get : comparable -> Node comparable v -> Maybe v
get key node =
    case node of
        Leaf list ->
            getFromLeaf key list

        NonLeaf n kn ->
            getFromNonLeaf key n kn


{-| assumes ordered list
-}
getFromLeaf : comparable -> List ( comparable, v ) -> Maybe v
getFromLeaf key list =
    case list of
        ( k, v ) :: list2 ->
            if key > k then
                getFromLeaf key list2
            else if key == k then
                Just v
            else
                Nothing

        [] ->
            Nothing


{-| assumes ordered list
-}
getFromNonLeaf : comparable -> Node comparable v -> List ( comparable, v, Node comparable v ) -> Maybe v
getFromNonLeaf key n kn =
    case kn of
        ( k, v, n2 ) :: kn2 ->
            if key > k then
                getFromNonLeaf key n2 kn2
            else if key == k then
                Just v
            else
                get key n

        [] ->
            get key n


type InsertResult k v
    = NoSplit (Node k v)
    | Split (Node k v) k v (Node k v)


insert : comparable -> v -> Node comparable v -> Node comparable v
insert key val node =
    case insertHelp key val node of
        NoSplit inserted ->
            inserted

        Split left k v right ->
            NonLeaf left [ ( k, v, right ) ]


insertHelp : comparable -> v -> Node comparable v -> InsertResult comparable v
insertHelp key val node =
    case node of
        Leaf list ->
            let
                list2 =
                    insertIntoLeaf key val list
            in
                if List.length list2 <= maxKeys then
                    NoSplit (Leaf list2)
                else
                    case bisect list2 of
                        ( llist, ( k, v ) :: rlist ) ->
                            Split (Leaf llist) k v (Leaf rlist)

                        _ ->
                            NoSplit (Leaf list2)

        NonLeaf n kn ->
            let
                ( n2, kn2 ) =
                    insertIntoNonLeaf key val [] n kn
            in
                if List.length kn2 <= maxKeys then
                    NoSplit (NonLeaf n2 kn2)
                else
                    case bisect kn2 of
                        ( lkn, ( k, v, r ) :: rkn ) ->
                            Split (NonLeaf n2 lkn) k v (NonLeaf r rkn)

                        _ ->
                            NoSplit (NonLeaf n2 kn2)


insertIntoLeaf : comparable -> v -> List ( comparable, v ) -> List ( comparable, v )
insertIntoLeaf key val list =
    case list of
        (( k, _ ) as kv) :: rest ->
            if key > k then
                kv :: insertIntoLeaf key val rest
            else if k == key then
                ( key, val ) :: rest
            else
                ( key, val ) :: list

        [] ->
            ( key, val ) :: []


insertIntoNonLeaf : comparable -> v -> NKList comparable v -> Node comparable v -> KNList comparable v -> NKNList comparable v
insertIntoNonLeaf key val nk n kn =
    case kn of
        ( k, v, n2 ) :: kn2 ->
            if key > k then
                -- keep unzipping
                insertIntoNonLeaf key val (( n, k, v ) :: nk) n2 kn2
            else if key == k then
                -- replace split-key
                zipNKNList nk n (( key, val, n2 ) :: kn2)
            else
                insertIntoNonLeafAtNode key val nk n kn

        [] ->
            insertIntoNonLeafAtNode key val nk n []


insertIntoNonLeafAtNode : comparable -> v -> NKList comparable v -> Node comparable v -> KNList comparable v -> NKNList comparable v
insertIntoNonLeafAtNode key val nk n kn =
    case insertHelp key val n of
        NoSplit n2 ->
            zipNKNList nk n2 kn

        Split l k v r ->
            zipNKNList nk l (( k, v, r ) :: kn)



-- type aliases (NKN == a list of nodes separated by keys)


type alias KNList k v =
    List ( k, v, Node k v )


type alias NKNList k v =
    ( Node k v, List ( k, v, Node k v ) )


type alias NKList k v =
    List ( Node k v, k, v )


zipNKNList : NKList comparable v -> Node comparable v -> KNList comparable v -> NKNList comparable v
zipNKNList nk n kn =
    case nk of
        [] ->
            ( n, kn )

        ( n2, k, v ) :: nk2 ->
            zipNKNList nk2 n2 (( k, v, n ) :: kn)


fromNKNList : NKNList comparable v -> Node comparable v
fromNKNList =
    uncurry NonLeaf



-- remove


{-| -}
remove : comparable -> Node comparable v -> Node comparable v
remove key node =
    case removeHelp key node of
        NonLeaf subnode [] ->
            -- decrease depth when the result has only one node
            subnode

        node2 ->
            node2


{-| resulting node may be underfull, but children will be balanced; the output will have the same depth as the input
-}
removeHelp : comparable -> Node comparable v -> Node comparable v
removeHelp key node =
    case node of
        Leaf list ->
            Leaf (removeFromLeaf key list)

        NonLeaf n kn ->
            fromNKNList (removeFromNonLeaf key [] n kn)


{-| resulting list may be underfull
-}
removeFromLeaf : comparable -> List ( comparable, v ) -> List ( comparable, v )
removeFromLeaf key list =
    case list of
        (( k, _ ) as kv) :: rest ->
            if key > k then
                kv :: removeFromLeaf key rest
            else if key == k then
                rest
            else
                list

        [] ->
            []


{-| resulting list may be underfull, but children will be balanced
-}
removeFromNonLeaf : comparable -> NKList comparable v -> Node comparable v -> KNList comparable v -> NKNList comparable v
removeFromNonLeaf key nk n kn =
    case kn of
        ( k, v, n2 ) :: kn2 ->
            if key > k then
                -- keep unzipping
                removeFromNonLeaf key (( n, k, v ) :: nk) n2 kn2
            else if key == k then
                -- remove split-key
                replaceSplitKey nk n n2 kn2
            else
                -- descend into node (leftmost or middle)
                rebalance nk (removeHelp key n) kn

        [] ->
            -- descend into node (rightmost)
            rebalance nk (removeHelp key n) []


{-| need siblings to rebalance a deficient node
-}
rebalance : NKList comparable v -> Node comparable v -> KNList comparable v -> NKNList comparable v
rebalance nk n kn =
    if keysInNode n >= minKeys then
        -- already balanced
        zipNKNList nk n kn
    else
        -- rebalance
        case ( nk, kn ) of
            ( ( l, lk, lv ) :: nk2, ( rk, rv, r ) :: kn2 ) ->
                -- central
                borrowFromRight nk n rk rv r kn2
                    |> orElseLazy (\() -> borrowFromLeft nk2 l lk lv n kn)
                    |> withDefaultLazy (\() -> mergeNodes nk2 l lk lv n kn)

            ( [], ( rk, rv, r ) :: kn2 ) ->
                -- leftmost
                borrowFromRight [] n rk rv r kn2
                    |> withDefaultLazy (\() -> mergeNodes [] n rk rv r kn2)

            ( ( l, lk, lv ) :: nk2, [] ) ->
                -- rightmost
                borrowFromLeft nk2 l lk lv n []
                    |> withDefaultLazy (\() -> mergeNodes nk2 l lk lv n [])

            ( [], [] ) ->
                -- singleton
                ( n, [] )


keysInNode : Node comparable v -> Int
keysInNode node =
    case node of
        Leaf list ->
            List.length list

        NonLeaf _ kn ->
            List.length kn


{-| rotate left
-}
borrowFromRight : NKList comparable v -> Node comparable v -> comparable -> v -> Node comparable v -> KNList comparable v -> Maybe (NKNList comparable v)
borrowFromRight nk l k v r kn =
    (case ( l, r ) of
        ( Leaf llist, Leaf (( rk, rv ) :: rlist2) ) ->
            if List.length rlist2 >= minKeys then
                Just ( Leaf (llist ++ [ ( k, v ) ]), rk, rv, Leaf rlist2 )
            else
                Nothing

        ( NonLeaf ln lkn, NonLeaf rn (( rk, rv, rn2 ) :: rkn2) ) ->
            if List.length rkn2 >= minKeys then
                Just ( NonLeaf ln (lkn ++ [ ( k, v, rn ) ]), rk, rv, NonLeaf rn2 rkn2 )
            else
                Nothing

        _ ->
            Nothing
    )
        -- reassemble
        |> Maybe.map (\( l2, k2, v2, r2 ) -> zipNKNList nk l2 (( k2, v2, r2 ) :: kn))


{-| rotate right
-}
borrowFromLeft : NKList comparable v -> Node comparable v -> comparable -> v -> Node comparable v -> KNList comparable v -> Maybe (NKNList comparable v)
borrowFromLeft nk l k v r kn =
    (case ( l, r ) of
        ( Leaf llist, Leaf rlist ) ->
            if List.length llist > minKeys then
                popLast llist
                    |> Maybe.map
                        (\( llist2, ( lk, lv ) ) ->
                            ( Leaf llist2, lk, lv, Leaf (( k, v ) :: rlist) )
                        )
            else
                Nothing

        ( NonLeaf ln lkn, NonLeaf rn rkn ) ->
            if List.length lkn > minKeys then
                popLast lkn
                    |> Maybe.map
                        (\( lkn2, ( lk, lv, lastNode ) ) ->
                            ( NonLeaf ln lkn2, lk, lv, NonLeaf lastNode (( k, v, rn ) :: rkn) )
                        )
            else
                Nothing

        _ ->
            Nothing
    )
        -- reassemble
        |> Maybe.map (\( l2, k2, v2, r2 ) -> zipNKNList nk l2 (( k2, v2, r2 ) :: kn))


mergeNodes : NKList comparable v -> Node comparable v -> comparable -> v -> Node comparable v -> KNList comparable v -> NKNList comparable v
mergeNodes nk l k v r kn =
    (case ( l, r ) of
        ( Leaf llist, Leaf rlist ) ->
            Leaf (llist ++ ( k, v ) :: rlist)

        ( NonLeaf ln lkn, NonLeaf rn rkn ) ->
            NonLeaf ln (lkn ++ ( k, v, rn ) :: rkn)

        _ ->
            Debug.crash "attempting to merge Leaf and NonLeaf nodes"
    )
        |> (\n -> zipNKNList nk n kn)


replaceSplitKey : NKList comparable v -> Node comparable v -> Node comparable v -> KNList comparable v -> NKNList comparable v
replaceSplitKey nk l r kn =
    case borrowSmallestKey r of
        Just ( k, v, r2 ) ->
            zipNKNList nk l (( k, v, r2 ) :: kn)

        Nothing ->
            case borrowLargestKey l of
                Just ( l2, k, v ) ->
                    zipNKNList nk l2 (( k, v, r ) :: kn)

                Nothing ->
                    if List.isEmpty kn then
                        case getLargestKey l of
                            Just ( k, v ) ->
                                rebalance nk (removeHelp k l) (( k, v, r ) :: kn)

                            Nothing ->
                                -- empty left node
                                -- zipNKNList nk r kn
                                Debug.crash "empty left node"
                    else
                        case getSmallestKey r of
                            Just ( k, v ) ->
                                rebalance (( l, k, v ) :: nk) (removeHelp k r) kn

                            Nothing ->
                                -- empty right node
                                -- zipNKNList nk l kn
                                Debug.crash "empty right node"


getSmallestKey : Node comparable v -> Maybe ( comparable, v )
getSmallestKey node =
    case node of
        Leaf list ->
            List.head list

        NonLeaf n _ ->
            getSmallestKey n


getLargestKey : Node comparable v -> Maybe ( comparable, v )
getLargestKey node =
    case node of
        Leaf list ->
            getLast list

        NonLeaf n [] ->
            getLargestKey n

        NonLeaf _ kn ->
            getLast kn |> Maybe.andThen (\( _, _, lastNode ) -> getLargestKey lastNode)


borrowSmallestKey : Node comparable v -> Maybe ( comparable, v, Node comparable v )
borrowSmallestKey node =
    case node of
        Leaf list ->
            if List.length list > minKeys then
                popFirst list |> Maybe.map (\( ( k, v ), list2 ) -> ( k, v, Leaf list2 ))
            else
                Nothing

        NonLeaf n kn ->
            borrowSmallestKey n |> Maybe.map (\( k, v, n2 ) -> ( k, v, NonLeaf n2 kn ))


borrowLargestKey : Node comparable v -> Maybe ( Node comparable v, comparable, v )
borrowLargestKey node =
    case node of
        Leaf list ->
            if List.length list > minKeys then
                popLast list |> Maybe.map (\( list2, ( k, v ) ) -> ( Leaf list2, k, v ))
            else
                Nothing

        NonLeaf n [] ->
            borrowLargestKey n |> Maybe.map (\( n2, k, v ) -> ( NonLeaf n2 [], k, v ))

        NonLeaf n kn ->
            popLast kn
                |> Maybe.andThen
                    (\( kn2, ( lastKey, lastVal, lastNode ) ) ->
                        borrowLargestKey lastNode |> Maybe.map (\( lastNode2, k, v ) -> ( NonLeaf n (kn2 ++ [ ( lastKey, lastVal, lastNode2 ) ]), k, v ))
                    )



-- foldl


foldl : (comparable -> v -> a -> a) -> a -> Node comparable v -> a
foldl f result node =
    case node of
        Leaf list ->
            foldlKList f result list

        NonLeaf n kn ->
            foldlKNList f (foldl f result n) kn


foldlKList : (comparable -> v -> a -> a) -> a -> List ( comparable, v ) -> a
foldlKList f result list =
    case list of
        ( k, v ) :: rest ->
            foldlKList f (f k v result) rest

        [] ->
            result


foldlKNList : (comparable -> v -> a -> a) -> a -> KNList comparable v -> a
foldlKNList f result kn =
    case kn of
        ( k, v, n ) :: kn2 ->
            foldlKNList f (foldl f (f k v result) n) kn2

        [] ->
            result



-- to/from List


{-| Naive implementation; consider bulkloading
-}
fromList : List ( comparable, v ) -> Node comparable v
fromList =
    List.foldl (uncurry insert) empty


{-| TODO compare this to foldr without reverse
-}
toList : Node comparable v -> List ( comparable, v )
toList =
    foldl (\k v r -> ( k, v ) :: r) []
        >> List.reverse



-- size


size : Node k v -> Int
size node =
    case node of
        Leaf list ->
            List.length list

        NonLeaf n kn ->
            sizeKNList (size n) kn


sizeKNList : Int -> List ( k, v, Node k v ) -> Int
sizeKNList r kn =
    case kn of
        ( _, _, n ) :: kn2 ->
            sizeKNList (r + 1 + size n) kn2

        [] ->
            r



-- Maybe functions


orElseLazy : (() -> Maybe a) -> Maybe a -> Maybe a
orElseLazy f m =
    case m of
        Just _ ->
            m

        Nothing ->
            f ()


withDefaultLazy : (() -> a) -> Maybe a -> a
withDefaultLazy f m =
    case m of
        Just x ->
            x

        Nothing ->
            f ()



-- List functions


getLast : List a -> Maybe a
getLast list =
    case list of
        _ :: ((_ :: _) as rest) ->
            getLast rest

        x :: rest ->
            Just x

        [] ->
            Nothing


{-| uncons
-}
popFirst : List a -> Maybe ( a, List a )
popFirst list =
    case list of
        head :: tail ->
            Just ( head, tail )

        [] ->
            Nothing


popLast : List a -> Maybe ( List a, a )
popLast list =
    case list of
        head :: tail ->
            Just (popLastHelp [] head tail)

        [] ->
            Nothing


popLastHelp : List a -> a -> List a -> ( List a, a )
popLastHelp rev x list =
    case list of
        [] ->
            ( List.reverse rev, x )

        y :: rest ->
            popLastHelp (x :: rev) y rest


{-| Split `list` into two halves, `( a, b )`, such that `a ++ b == list`.

    bisect [ 1, 2, 3, 4 ] == ( [ 1, 2 ], [ 3, 4 ] )

If the list is odd in length, then the first half will be the shorter of the halves.

    bisect [ 1, 2, 3 ] == ( [ 1 ], [ 2, 3 ] )

-}
bisect : List a -> ( List a, List a )
bisect list =
    bisectHelp (List.length list // 2) [] list


bisectHelp : Int -> List a -> List a -> ( List a, List a )
bisectHelp i rev list =
    if i <= 0 then
        ( List.reverse rev, list )
    else
        case list of
            x :: rest ->
                bisectHelp (i - 1) (x :: rev) rest

            _ ->
                ( [], [] )
