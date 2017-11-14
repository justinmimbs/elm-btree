module Dict.BTree
    exposing
        ( Dict(..)
        , empty
        , singleton
        , isEmpty
        , size
        , get
        , member
        , insert
        , remove
        , update
        , map
        , foldl
        , foldr
        , filter
        , partition
        , union
        , intersect
        , diff
        , merge
        , keys
        , values
        , fromList
        , toList
        )

{-| Order-4 B-tree
-}


type Dict k v
    = Leaf
    | N2 (Dict k v) ( k, v ) (Dict k v)
    | N3 (Dict k v) ( k, v ) (Dict k v) ( k, v ) (Dict k v)
    | N4 (Dict k v) ( k, v ) (Dict k v) ( k, v ) (Dict k v) ( k, v ) (Dict k v)


empty : Dict k v
empty =
    Leaf


singleton : k -> v -> Dict k v
singleton key val =
    N2 Leaf ( key, val ) Leaf


isEmpty : Dict k v -> Bool
isEmpty =
    (==) Leaf


size : Dict k v -> Int
size node =
    case node of
        Leaf ->
            0

        N2 a _ b ->
            1 + size a + size b

        N3 a _ b _ c ->
            2 + size a + size b + size c

        N4 a _ b _ c _ d ->
            3 + size a + size b + size c + size d



-- get


get : comparable -> Dict comparable v -> Maybe v
get key node =
    case node of
        Leaf ->
            Nothing

        N2 a ( k1, v1 ) b ->
            if key < k1 then
                get key a
            else if key > k1 then
                get key b
            else
                Just v1

        N3 a ( k1, v1 ) b ( k2, v2 ) c ->
            if key < k2 then
                if key < k1 then
                    get key a
                else if key > k1 then
                    get key b
                else
                    Just v1
            else if key > k2 then
                get key c
            else
                Just v2

        N4 a ( k1, v1 ) b ( k2, v2 ) c ( k3, v3 ) d ->
            if key < k2 then
                if key < k1 then
                    get key a
                else if key > k1 then
                    get key b
                else
                    Just v1
            else if key > k2 then
                if key < k3 then
                    get key c
                else if key > k3 then
                    get key d
                else
                    Just v3
            else
                Just v2



-- member


member : comparable -> Dict comparable v -> Bool
member key node =
    case get key node of
        Just _ ->
            True

        Nothing ->
            False



-- zipping


{-| Represents a tree "unzipped" to the location of a specific node or key
within a tree. A `Location` contains the node or key under focus as well as
the context needed to "zip" the tree back up.
-}
type Location k v
    = NodeLoc (Dict k v) (NodePath k v)
    | KeyLoc ( k, v ) (KeyContext k v) (NodePath k v)


{-| Represents the context surrounding a node within a tree.
-}
type NodePath k v
    = Top
    | Subnode (NodeContext k v) (NodePath k v)


{-| Represents the context surrounding a subnode within a node.
-}
type NodeContext k v
    = N2A ( k, v ) (Dict k v)
    | N2B (Dict k v) ( k, v )
    | N3A ( k, v ) (Dict k v) ( k, v ) (Dict k v)
    | N3B (Dict k v) ( k, v ) ( k, v ) (Dict k v)
    | N3C (Dict k v) ( k, v ) (Dict k v) ( k, v )
    | N4A ( k, v ) (Dict k v) ( k, v ) (Dict k v) ( k, v ) (Dict k v)
    | N4B (Dict k v) ( k, v ) ( k, v ) (Dict k v) ( k, v ) (Dict k v)
    | N4C (Dict k v) ( k, v ) (Dict k v) ( k, v ) ( k, v ) (Dict k v)
    | N4D (Dict k v) ( k, v ) (Dict k v) ( k, v ) (Dict k v) ( k, v )


{-| Represents the context surrounding a key within a node.
-}
type KeyContext k v
    = N2P1 (Dict k v) (Dict k v)
    | N3P1 (Dict k v) (Dict k v) ( k, v ) (Dict k v)
    | N3P2 (Dict k v) ( k, v ) (Dict k v) (Dict k v)
    | N4P1 (Dict k v) (Dict k v) ( k, v ) (Dict k v) ( k, v ) (Dict k v)
    | N4P2 (Dict k v) ( k, v ) (Dict k v) (Dict k v) ( k, v ) (Dict k v)
    | N4P3 (Dict k v) ( k, v ) (Dict k v) ( k, v ) (Dict k v) (Dict k v)


seek : comparable -> NodePath comparable v -> Dict comparable v -> Location comparable v
seek key path node =
    case node of
        Leaf ->
            NodeLoc node path

        N2 a (( k1, _ ) as p1) b ->
            if key < k1 then
                a |> seek key (Subnode (N2A p1 b) path)
            else if key > k1 then
                b |> seek key (Subnode (N2B a p1) path)
            else
                KeyLoc p1 (N2P1 a b) path

        N3 a (( k1, _ ) as p1) b (( k2, _ ) as p2) c ->
            if key < k2 then
                if key < k1 then
                    a |> seek key (Subnode (N3A p1 b p2 c) path)
                else if key > k1 then
                    b |> seek key (Subnode (N3B a p1 p2 c) path)
                else
                    KeyLoc p1 (N3P1 a b p2 c) path
            else if key > k2 then
                c |> seek key (Subnode (N3C a p1 b p2) path)
            else
                KeyLoc p2 (N3P2 a p1 b c) path

        N4 a (( k1, _ ) as p1) b (( k2, _ ) as p2) c (( k3, _ ) as p3) d ->
            if key < k2 then
                if key < k1 then
                    a |> seek key (Subnode (N4A p1 b p2 c p3 d) path)
                else if key > k1 then
                    b |> seek key (Subnode (N4B a p1 p2 c p3 d) path)
                else
                    KeyLoc p1 (N4P1 a b p2 c p3 d) path
            else if key > k2 then
                if key < k3 then
                    c |> seek key (Subnode (N4C a p1 b p2 p3 d) path)
                else if key > k3 then
                    d |> seek key (Subnode (N4D a p1 b p2 c p3) path)
                else
                    KeyLoc p3 (N4P3 a p1 b p2 c d) path
            else
                KeyLoc p2 (N4P2 a p1 b c p3 d) path


zipNode : NodePath k v -> Dict k v -> Dict k v
zipNode path node =
    case path of
        Top ->
            node

        Subnode nodeContext nextPath ->
            node |> wrapNode nodeContext |> zipNode nextPath


wrapNode : NodeContext k v -> Dict k v -> Dict k v
wrapNode nodeContext node =
    case nodeContext of
        N2A p1 b ->
            N2 node p1 b

        N2B a p1 ->
            N2 a p1 node

        N3A p1 b p2 c ->
            N3 node p1 b p2 c

        N3B a p1 p2 c ->
            N3 a p1 node p2 c

        N3C a p1 b p2 ->
            N3 a p1 b p2 node

        N4A p1 b p2 c p3 d ->
            N4 node p1 b p2 c p3 d

        N4B a p1 p2 c p3 d ->
            N4 a p1 node p2 c p3 d

        N4C a p1 b p2 p3 d ->
            N4 a p1 b p2 node p3 d

        N4D a p1 b p2 c p3 ->
            N4 a p1 b p2 c p3 node


wrapKey : KeyContext k v -> ( k, v ) -> Dict k v
wrapKey keyContext pair =
    case keyContext of
        N2P1 a b ->
            N2 a pair b

        N3P1 a b p2 c ->
            N3 a pair b p2 c

        N3P2 a p1 b c ->
            N3 a p1 b pair c

        N4P1 a b p2 c p3 d ->
            N4 a pair b p2 c p3 d

        N4P2 a p1 b c p3 d ->
            N4 a p1 b pair c p3 d

        N4P3 a p1 b p2 c d ->
            N4 a p1 b p2 c pair d


{-| Represents the possible outcomes of inserting into a node.
-}
type InsertResult k v
    = BalancedInsert (Dict k v)
    | Split (Dict k v) ( k, v ) (Dict k v)


{-| May change the depth of the tree.
-}
fromInsertResult : InsertResult k v -> Dict k v
fromInsertResult nodeResult =
    case nodeResult of
        BalancedInsert node ->
            -- maintains depth
            node

        Split left pair right ->
            -- increases depth
            N2 left pair right


zipInsertResult : NodePath comparable v -> InsertResult comparable v -> InsertResult comparable v
zipInsertResult path nodeResult =
    case path of
        Top ->
            nodeResult

        Subnode nodeContext pathNext ->
            case nodeResult of
                BalancedInsert node ->
                    BalancedInsert (node |> wrapNode nodeContext |> zipNode pathNext)

                _ ->
                    nodeResult |> wrapInsertResult nodeContext |> zipInsertResult pathNext


wrapInsertResult : NodeContext comparable v -> InsertResult comparable v -> InsertResult comparable v
wrapInsertResult nodeContext nodeResult =
    case nodeResult of
        BalancedInsert node ->
            BalancedInsert (node |> wrapNode nodeContext)

        Split left pair right ->
            case nodeContext of
                N2A p1 b ->
                    BalancedInsert (N3 left pair right p1 b)

                N2B a p1 ->
                    BalancedInsert (N3 a p1 left pair right)

                N3A p1 b p2 c ->
                    BalancedInsert (N4 left pair right p1 b p2 c)

                N3B a p1 p2 c ->
                    BalancedInsert (N4 a p1 left pair right p2 c)

                N3C a p1 b p2 ->
                    BalancedInsert (N4 a p1 b p2 left pair right)

                N4A p1 b p2 c p3 d ->
                    Split (N3 left pair right p1 b) p2 (N2 c p3 d)

                N4B a p1 p2 c p3 d ->
                    Split (N3 a p1 left pair right) p2 (N2 c p3 d)

                N4C a p1 b p2 p3 d ->
                    Split (N3 a p1 b p2 left) pair (N2 right p3 d)

                N4D a p1 b p2 c p3 ->
                    Split (N3 a p1 b p2 c) p3 (N2 left pair right)


{-| Represents the possible outcomes of removing from a node.
-}
type RemoveResult k v
    = BalancedRemove (Dict k v)
    | Underfull (Dict k v)


{-| May change the depth of the tree.
-}
fromRemoveResult : RemoveResult k v -> Dict k v
fromRemoveResult nodeResult =
    case nodeResult of
        BalancedRemove node ->
            -- maintains depth
            node

        Underfull subnode ->
            -- decreases depth
            subnode


zipRemoveResult : NodePath comparable v -> RemoveResult comparable v -> RemoveResult comparable v
zipRemoveResult path nodeResult =
    case path of
        Top ->
            nodeResult

        Subnode nodeContext pathNext ->
            case nodeResult of
                BalancedRemove node ->
                    BalancedRemove (node |> wrapNode nodeContext |> zipNode pathNext)

                _ ->
                    nodeResult |> wrapRemoveResult nodeContext |> zipRemoveResult pathNext


wrapRemoveResult : NodeContext comparable v -> RemoveResult comparable v -> RemoveResult comparable v
wrapRemoveResult nodeContext nodeResult =
    case nodeResult of
        BalancedRemove node ->
            BalancedRemove (node |> wrapNode nodeContext)

        Underfull subnode ->
            case nodeContext of
                N2A p1 b ->
                    -- leftmost
                    case rotateLeft subnode p1 b of
                        Rotated newA newP1 newB ->
                            BalancedRemove (N2 newA newP1 newB)

                        Merged ab ->
                            Underfull ab

                N2B a p1 ->
                    -- rightmost
                    case rotateRight a p1 subnode of
                        Rotated newA newP1 newB ->
                            BalancedRemove (N2 newA newP1 newB)

                        Merged ab ->
                            Underfull ab

                N3A p1 b p2 c ->
                    -- leftmost
                    case rotateLeft subnode p1 b of
                        Rotated newA newP1 newB ->
                            BalancedRemove (N3 newA newP1 newB p2 c)

                        Merged ab ->
                            BalancedRemove (N2 ab p2 c)

                N3B a p1 p2 c ->
                    -- middle
                    case rotateLeft subnode p2 c of
                        Rotated newB newP2 newC ->
                            BalancedRemove (N3 a p1 newB newP2 newC)

                        _ ->
                            case rotateRight a p1 subnode of
                                Rotated newA newP1 newB ->
                                    BalancedRemove (N3 newA newP1 newB p2 c)

                                Merged ab ->
                                    BalancedRemove (N2 ab p2 c)

                N3C a p1 b p2 ->
                    -- rightmost
                    case rotateRight b p2 subnode of
                        Rotated newB newP2 newC ->
                            BalancedRemove (N3 a p1 newB newP2 newC)

                        Merged bc ->
                            BalancedRemove (N2 a p1 bc)

                N4A p1 b p2 c p3 d ->
                    -- leftmost
                    case rotateLeft subnode p1 b of
                        Rotated newA newP1 newB ->
                            BalancedRemove (N4 newA newP1 newB p2 c p3 d)

                        Merged ab ->
                            BalancedRemove (N3 ab p2 c p3 d)

                N4B a p1 p2 c p3 d ->
                    -- middle
                    case rotateLeft subnode p2 c of
                        Rotated newB newP2 newC ->
                            BalancedRemove (N4 a p1 newB newP2 newC p3 d)

                        _ ->
                            case rotateRight a p1 subnode of
                                Rotated newA newP1 newB ->
                                    BalancedRemove (N4 newA newP1 newB p2 c p3 d)

                                Merged ab ->
                                    BalancedRemove (N3 ab p2 c p3 d)

                N4C a p1 b p2 p3 d ->
                    -- middle
                    case rotateLeft subnode p3 d of
                        Rotated newC newP3 newD ->
                            BalancedRemove (N4 a p1 b p2 newC newP3 newD)

                        _ ->
                            case rotateRight b p2 subnode of
                                Rotated newB newP2 newC ->
                                    BalancedRemove (N4 a p1 newB newP2 newC p3 d)

                                Merged bc ->
                                    BalancedRemove (N3 a p1 bc p3 d)

                N4D a p1 b p2 c p3 ->
                    -- rightmost
                    case rotateRight c p3 subnode of
                        Rotated newC newP3 newD ->
                            BalancedRemove (N4 a p1 b p2 newC newP3 newD)

                        Merged cd ->
                            BalancedRemove (N3 a p1 b p2 cd)


wrapWithoutKey : KeyContext comparable v -> RemoveResult comparable v
wrapWithoutKey keyContext =
    case keyContext of
        N2P1 a b ->
            case findReplacementKey a b of
                Rotated newA newP1 newB ->
                    BalancedRemove (N2 newA newP1 newB)

                Merged ab ->
                    Underfull ab

        N3P1 a b p2 c ->
            case findReplacementKey a b of
                Rotated newA newP1 newB ->
                    BalancedRemove (N3 newA newP1 newB p2 c)

                Merged ab ->
                    BalancedRemove (N2 ab p2 c)

        N3P2 a p1 b c ->
            case findReplacementKey b c of
                Rotated newB newP2 newC ->
                    BalancedRemove (N3 a p1 newB newP2 newC)

                Merged bc ->
                    BalancedRemove (N2 a p1 bc)

        N4P1 a b p2 c p3 d ->
            case findReplacementKey a b of
                Rotated newA newP1 newB ->
                    BalancedRemove (N4 newA newP1 newB p2 c p3 d)

                Merged ab ->
                    BalancedRemove (N3 ab p2 c p3 d)

        N4P2 a p1 b c p3 d ->
            case findReplacementKey b c of
                Rotated newB newP2 newC ->
                    BalancedRemove (N4 a p1 newB newP2 newC p3 d)

                Merged bc ->
                    BalancedRemove (N3 a p1 bc p3 d)

        N4P3 a p1 b p2 c d ->
            case findReplacementKey c d of
                Rotated newC newP3 newD ->
                    BalancedRemove (N4 a p1 b p2 newC newP3 newD)

                Merged cd ->
                    BalancedRemove (N3 a p1 b p2 cd)


type RebalanceResult k v
    = Rotated (Dict k v) ( k, v ) (Dict k v)
    | Merged (Dict k v)


rotateLeft : Dict k v -> ( k, v ) -> Dict k v -> RebalanceResult k v
rotateLeft subnode pair right =
    case right of
        Leaf ->
            Merged Leaf

        N2 a p1 b ->
            Merged (N3 subnode pair a p1 b)

        N3 a p1 b p2 c ->
            Rotated (N2 subnode pair a) p1 (N2 b p2 c)

        N4 a p1 b p2 c p3 d ->
            Rotated (N2 subnode pair a) p1 (N3 b p2 c p3 d)


rotateRight : Dict k v -> ( k, v ) -> Dict k v -> RebalanceResult k v
rotateRight left pair subnode =
    case left of
        Leaf ->
            Merged Leaf

        N2 a p1 b ->
            Merged (N3 a p1 b pair subnode)

        N3 a p1 b p2 c ->
            Rotated (N2 a p1 b) p2 (N2 c pair subnode)

        N4 a p1 b p2 c p3 d ->
            Rotated (N3 a p1 b p2 c) p3 (N2 d pair subnode)


findReplacementKey : Dict comparable v -> Dict comparable v -> RebalanceResult comparable v
findReplacementKey left right =
    case borrowSmallest right of
        Just ( pair, newRight ) ->
            Rotated left pair newRight

        Nothing ->
            case borrowLargest left of
                Just ( newLeft, pair ) ->
                    Rotated newLeft pair right

                Nothing ->
                    case getSmallest Nothing right of
                        Just (( key, _ ) as pair) ->
                            case removeFromNode key right of
                                BalancedRemove newRight ->
                                    Rotated left pair newRight

                                Underfull rightSubnode ->
                                    rotateRight left pair rightSubnode

                        Nothing ->
                            Merged Leaf


borrowSmallest : Dict k v -> Maybe ( ( k, v ), Dict k v )
borrowSmallest node =
    case node of
        Leaf ->
            Nothing

        N2 a p1 b ->
            if a == Leaf then
                Nothing
            else
                borrowSmallest a |> Maybe.map (\( smallest, aNew ) -> ( smallest, N2 aNew p1 b ))

        N3 a p1 b p2 c ->
            if a == Leaf then
                Just ( p1, N2 Leaf p2 Leaf )
            else
                borrowSmallest a |> Maybe.map (\( smallest, aNew ) -> ( smallest, N3 aNew p1 b p2 c ))

        N4 a p1 b p2 c p3 d ->
            if a == Leaf then
                Just ( p1, N3 Leaf p2 Leaf p3 Leaf )
            else
                borrowSmallest a |> Maybe.map (\( smallest, aNew ) -> ( smallest, N4 aNew p1 b p2 c p3 d ))


borrowLargest : Dict k v -> Maybe ( Dict k v, ( k, v ) )
borrowLargest node =
    case node of
        Leaf ->
            Nothing

        N2 a p1 b ->
            if b == Leaf then
                Nothing
            else
                borrowLargest b |> Maybe.map (\( bNew, largest ) -> ( N2 a p1 bNew, largest ))

        N3 a p1 b p2 c ->
            if c == Leaf then
                Just ( N2 Leaf p1 Leaf, p2 )
            else
                borrowLargest c |> Maybe.map (\( cNew, largest ) -> ( N3 a p1 b p2 cNew, largest ))

        N4 a p1 b p2 c p3 d ->
            if d == Leaf then
                Just ( N3 Leaf p1 Leaf p2 Leaf, p3 )
            else
                borrowLargest d |> Maybe.map (\( dNew, largest ) -> ( N4 a p1 b p2 c p3 dNew, largest ))


getSmallest : Maybe ( k, v ) -> Dict k v -> Maybe ( k, v )
getSmallest smallest node =
    case node of
        Leaf ->
            smallest

        N2 a p1 _ ->
            getSmallest (Just p1) a

        N3 a p1 _ _ _ ->
            getSmallest (Just p1) a

        N4 a p1 _ _ _ _ _ ->
            getSmallest (Just p1) a


getLargest : Maybe ( k, v ) -> Dict k v -> Maybe ( k, v )
getLargest largest node =
    case node of
        Leaf ->
            largest

        N2 _ p1 b ->
            getLargest (Just p1) b

        N3 _ _ _ p2 c ->
            getLargest (Just p2) c

        N4 _ _ _ _ _ p3 d ->
            getLargest (Just p3) d



-- insert


insert : comparable -> v -> Dict comparable v -> Dict comparable v
insert key val dict =
    case dict |> seek key Top of
        NodeLoc _ path ->
            Split Leaf ( key, val ) Leaf |> zipInsertResult path |> fromInsertResult

        KeyLoc _ keyContext path ->
            ( key, val ) |> wrapKey keyContext |> zipNode path



-- remove


remove : comparable -> Dict comparable v -> Dict comparable v
remove key =
    removeFromNode key >> fromRemoveResult


removeFromNode : comparable -> Dict comparable v -> RemoveResult comparable v
removeFromNode key node =
    case node |> seek key Top of
        NodeLoc _ _ ->
            BalancedRemove node

        KeyLoc _ keyContext path ->
            wrapWithoutKey keyContext |> zipRemoveResult path



-- update


update : comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v
update key f dict =
    case dict |> seek key Top of
        NodeLoc _ path ->
            case f Nothing of
                Just val ->
                    Split Leaf ( key, val ) Leaf |> zipInsertResult path |> fromInsertResult

                Nothing ->
                    dict

        KeyLoc ( _, val ) keyContext path ->
            case f (Just val) of
                Just newVal ->
                    ( key, newVal ) |> wrapKey keyContext |> zipNode path

                Nothing ->
                    wrapWithoutKey keyContext |> zipRemoveResult path |> fromRemoveResult



-- map


map : (k -> a -> b) -> Dict k a -> Dict k b
map f node =
    case node of
        Leaf ->
            Leaf

        N2 a ( k1, v1 ) b ->
            N2 (map f a) ( k1, f k1 v1 ) (map f b)

        N3 a ( k1, v1 ) b ( k2, v2 ) c ->
            N3 (map f a) ( k1, f k1 v1 ) (map f b) ( k2, f k2 v2 ) (map f c)

        N4 a ( k1, v1 ) b ( k2, v2 ) c ( k3, v3 ) d ->
            N4 (map f a) ( k1, f k1 v1 ) (map f b) ( k2, f k2 v2 ) (map f c) ( k3, f k3 v3 ) (map f d)



-- fold


foldl : (k -> v -> a -> a) -> a -> Dict k v -> a
foldl f result node =
    case node of
        Leaf ->
            result

        N2 a ( k1, v1 ) b ->
            foldl f (f k1 v1 (foldl f result a)) b

        N3 a ( k1, v1 ) b ( k2, v2 ) c ->
            foldl f (f k2 v2 (foldl f (f k1 v1 (foldl f result a)) b)) c

        N4 a ( k1, v1 ) b ( k2, v2 ) c ( k3, v3 ) d ->
            foldl f (f k3 v3 (foldl f (f k2 v2 (foldl f (f k1 v1 (foldl f result a)) b)) c)) d


foldr : (k -> v -> a -> a) -> a -> Dict k v -> a
foldr f result node =
    case node of
        Leaf ->
            result

        N2 a ( k1, v1 ) b ->
            foldr f (f k1 v1 (foldr f result b)) a

        N3 a ( k1, v1 ) b ( k2, v2 ) c ->
            foldr f (f k1 v1 (foldr f (f k2 v2 (foldr f result c)) b)) a

        N4 a ( k1, v1 ) b ( k2, v2 ) c ( k3, v3 ) d ->
            foldr f (f k1 v1 (foldr f (f k2 v2 (foldr f (f k3 v3 (foldr f result d)) c)) b)) a


{-| Alternate foldr where the mapping function takes a key-value tuple
-}
foldr_ : (( k, v ) -> a -> a) -> a -> Dict k v -> a
foldr_ f result node =
    case node of
        Leaf ->
            result

        N2 a p1 b ->
            foldr_ f (f p1 (foldr_ f result b)) a

        N3 a p1 b p2 c ->
            foldr_ f (f p1 (foldr_ f (f p2 (foldr_ f result c)) b)) a

        N4 a p1 b p2 c p3 d ->
            foldr_ f (f p1 (foldr_ f (f p2 (foldr_ f (f p3 (foldr_ f result d)) c)) b)) a



-- filter, partition


filter : (k -> v -> Bool) -> Dict k v -> Dict k v
filter pred =
    foldr_
        (\(( key, val ) as pair) list ->
            if pred key val then
                pair :: list
            else
                list
        )
        []
        >> fromSortedList True


partition : (k -> v -> Bool) -> Dict k v -> ( Dict k v, Dict k v )
partition pred =
    foldr_
        (\(( key, val ) as pair) ( list1, list2 ) ->
            if pred key val then
                ( pair :: list1, list2 )
            else
                ( list1, pair :: list2 )
        )
        ( [], [] )
        >> (\( list1, list2 ) -> ( fromSortedList True list1, fromSortedList True list2 ))



-- combine


union : Dict comparable v -> Dict comparable v -> Dict comparable v
union left right =
    case ( left, right ) of
        ( _, Leaf ) ->
            left

        ( Leaf, right ) ->
            right

        _ ->
            foldl unionAccumulator ( [], toList right ) left |> uncurry (List.foldl (::)) |> fromSortedList False


unionAccumulator : comparable -> v -> ( List ( comparable, v ), List ( comparable, v ) ) -> ( List ( comparable, v ), List ( comparable, v ) )
unionAccumulator lKey lVal ( result, rList ) =
    case rList of
        [] ->
            ( ( lKey, lVal ) :: result, [] )

        ( rKey, rVal ) :: rRest ->
            if lKey == rKey then
                ( ( lKey, lVal ) :: result, rRest )
            else if lKey < rKey then
                ( ( lKey, lVal ) :: result, rList )
            else
                ( ( rKey, rVal ) :: result, rRest ) |> unionAccumulator lKey lVal


intersect : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect left right =
    case ( getRange left, getRange right ) of
        ( _, Nothing ) ->
            empty

        ( Nothing, _ ) ->
            empty

        ( Just ( lMin, lMax ), Just ( rMin, rMax ) ) ->
            if lMax < rMin || rMax < lMin then
                -- disjoint ranges
                empty
            else
                foldl intersectAccumulator ( [], toList right ) left |> Tuple.first |> fromSortedList False


intersectAccumulator : comparable -> v -> ( List ( comparable, v ), List ( comparable, v ) ) -> ( List ( comparable, v ), List ( comparable, v ) )
intersectAccumulator lKey lVal (( result, rList ) as return) =
    case rList of
        [] ->
            return

        ( rKey, rVal ) :: rRest ->
            if lKey == rKey then
                ( ( lKey, lVal ) :: result, rRest )
            else if lKey < rKey then
                return
            else
                ( result, rRest ) |> intersectAccumulator lKey lVal


diff : Dict comparable v -> Dict comparable v -> Dict comparable v
diff left right =
    case ( getRange left, getRange right ) of
        ( _, Nothing ) ->
            left

        ( Nothing, _ ) ->
            empty

        ( Just ( lMin, lMax ), Just ( rMin, rMax ) ) ->
            if lMax < rMin || rMax < lMin then
                -- disjoint ranges
                left
            else
                foldl diffAccumulator ( [], toList right ) left |> Tuple.first |> fromSortedList False


diffAccumulator : comparable -> v -> ( List ( comparable, v ), List ( comparable, v ) ) -> ( List ( comparable, v ), List ( comparable, v ) )
diffAccumulator lKey lVal ( result, rList ) =
    case rList of
        [] ->
            ( ( lKey, lVal ) :: result, [] )

        ( rKey, rVal ) :: rRest ->
            if lKey == rKey then
                ( result, rRest )
            else if lKey < rKey then
                ( ( lKey, lVal ) :: result, rList )
            else
                ( result, rRest ) |> diffAccumulator lKey lVal



-- range


getRange : Dict comparable v -> Maybe ( comparable, comparable )
getRange node =
    case ( getSmallest Nothing node, getLargest Nothing node ) of
        ( Just ( minKey, _ ), Just ( maxKey, _ ) ) ->
            Just ( minKey, maxKey )

        _ ->
            Nothing



-- merge


merge :
    (comparable -> a -> result -> result)
    -> (comparable -> a -> b -> result -> result)
    -> (comparable -> b -> result -> result)
    -> Dict comparable a
    -> Dict comparable b
    -> result
    -> result
merge accLeft accBoth accRight left right result0 =
    let
        accumulate : comparable -> a -> ( result, List ( comparable, b ) ) -> ( result, List ( comparable, b ) )
        accumulate lKey lVal ( result, rList ) =
            case rList of
                [] ->
                    ( accLeft lKey lVal result, [] )

                ( rKey, rVal ) :: rRest ->
                    if lKey == rKey then
                        ( accBoth lKey lVal rVal result, rRest )
                    else if lKey < rKey then
                        ( accLeft lKey lVal result, rList )
                    else
                        ( accRight rKey rVal result, rRest ) |> accumulate lKey lVal
    in
        foldl accumulate ( result0, toList right ) left |> uncurry (List.foldl (uncurry accRight))



-- list


keys : Dict k v -> List k
keys =
    foldr (\k _ -> (::) k) []


values : Dict k v -> List v
values =
    foldr (\_ v -> (::) v) []


toList : Dict k v -> List ( k, v )
toList =
    foldr_ (::) []


fromList : List ( comparable, v ) -> Dict comparable v
fromList =
    List.sortBy Tuple.first >> removeRepeats >> fromSortedList False


{-| Remove consecutive duplicates, where last duplicate wins. (reverses order)
-}
removeRepeats : List ( comparable, v ) -> List ( comparable, v )
removeRepeats list =
    case list of
        pair :: rest ->
            removeRepeatsHelp [] pair rest

        [] ->
            []


removeRepeatsHelp : List ( comparable, v ) -> ( comparable, v ) -> List ( comparable, v ) -> List ( comparable, v )
removeRepeatsHelp revList (( key, _ ) as pair) list =
    case list of
        (( nextKey, _ ) as nextPair) :: rest ->
            if key == nextKey then
                removeRepeatsHelp revList nextPair rest
            else
                removeRepeatsHelp (pair :: revList) nextPair rest

        [] ->
            pair :: revList


{-| Convert an association list with sorted and distinct keys into a dictionary.
-}
fromSortedList : Bool -> List ( k, v ) -> Dict k v
fromSortedList isAsc list =
    case list of
        [] ->
            Leaf

        pair :: rest ->
            if isAsc then
                sortedListToNodeListAsc [] pair rest |> fromNodeList True
            else
                sortedListToNodeListDesc [] pair rest |> fromNodeList False


{-| Represents a non-empty list of nodes separated by key-value pairs.
-}
type alias NodeList k v =
    ( Dict k v, List ( ( k, v ), Dict k v ) )


{-| Convert a non-empty association list to the bottom level of nodes separated
by key-value pairs. (reverses order)
-}
sortedListToNodeListAsc : List ( ( k, v ), Dict k v ) -> ( k, v ) -> List ( k, v ) -> NodeList k v
sortedListToNodeListAsc revList p1 list =
    case list of
        [] ->
            ( N2 Leaf p1 Leaf, revList )

        p2 :: [] ->
            ( N3 Leaf p1 Leaf p2 Leaf, revList )

        p2 :: p3 :: [] ->
            ( N4 Leaf p1 Leaf p2 Leaf p3 Leaf, revList )

        p2 :: p3 :: p4 :: [] ->
            ( N2 Leaf p4 Leaf, ( p3, N3 Leaf p1 Leaf p2 Leaf ) :: revList )

        p2 :: p3 :: p4 :: p5 :: rest ->
            sortedListToNodeListAsc (( p4, N4 Leaf p1 Leaf p2 Leaf p3 Leaf ) :: revList) p5 rest


sortedListToNodeListDesc : List ( ( k, v ), Dict k v ) -> ( k, v ) -> List ( k, v ) -> NodeList k v
sortedListToNodeListDesc revList p1 list =
    case list of
        [] ->
            ( N2 Leaf p1 Leaf, revList )

        p2 :: [] ->
            ( N3 Leaf p2 Leaf p1 Leaf, revList )

        p2 :: p3 :: [] ->
            ( N4 Leaf p3 Leaf p2 Leaf p1 Leaf, revList )

        p2 :: p3 :: p4 :: [] ->
            ( N2 Leaf p4 Leaf, ( p3, N3 Leaf p2 Leaf p1 Leaf ) :: revList )

        p2 :: p3 :: p4 :: p5 :: rest ->
            sortedListToNodeListDesc (( p4, N4 Leaf p3 Leaf p2 Leaf p1 Leaf ) :: revList) p5 rest


{-| Gather up a NodeList one level at a time, in successive passes of alternating
direction, until a single root-node remains.
-}
fromNodeList : Bool -> NodeList k v -> Dict k v
fromNodeList isReversed nodeList =
    case nodeList of
        ( node, [] ) ->
            node

        ( a, ( p1, b ) :: list ) ->
            accumulateNodeList isReversed [] a p1 b list
                |> fromNodeList (not isReversed)


{-| Gather up a NodeList to the next level. (reverses order)
-}
accumulateNodeList : Bool -> List ( ( k, v ), Dict k v ) -> Dict k v -> ( k, v ) -> Dict k v -> List ( ( k, v ), Dict k v ) -> NodeList k v
accumulateNodeList isReversed revList a p1 b list =
    case list of
        [] ->
            if isReversed then
                ( N2 b p1 a, revList )
            else
                ( N2 a p1 b, revList )

        ( p2, c ) :: [] ->
            if isReversed then
                ( N3 c p2 b p1 a, revList )
            else
                ( N3 a p1 b p2 c, revList )

        ( p2, c ) :: ( p3, d ) :: [] ->
            if isReversed then
                ( N4 d p3 c p2 b p1 a, revList )
            else
                ( N4 a p1 b p2 c p3 d, revList )

        ( p2, c ) :: ( p3, d ) :: ( p4, e ) :: [] ->
            if isReversed then
                ( N2 e p4 d, ( p3, N3 c p2 b p1 a ) :: revList )
            else
                ( N2 d p4 e, ( p3, N3 a p1 b p2 c ) :: revList )

        ( p2, c ) :: ( p3, d ) :: ( p4, e ) :: ( p5, f ) :: rest ->
            if isReversed then
                accumulateNodeList isReversed (( p4, N4 d p3 c p2 b p1 a ) :: revList) e p5 f rest
            else
                accumulateNodeList isReversed (( p4, N4 a p1 b p2 c p3 d ) :: revList) e p5 f rest
