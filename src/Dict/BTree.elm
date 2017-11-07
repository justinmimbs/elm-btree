module Dict.BTree
    exposing
        ( Node(..)
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
        , keys
        , values
        , fromList
        , toList
        )

{-| Order-4 B-tree
-}


type Node k v
    = Leaf
    | N2 (Node k v) ( k, v ) (Node k v)
    | N3 (Node k v) ( k, v ) (Node k v) ( k, v ) (Node k v)
    | N4 (Node k v) ( k, v ) (Node k v) ( k, v ) (Node k v) ( k, v ) (Node k v)


empty : Node k v
empty =
    Leaf


singleton : comparable -> v -> Node comparable v
singleton key val =
    N2 Leaf ( key, val ) Leaf


isEmpty : Node k v -> Bool
isEmpty =
    (==) Leaf


size : Node k v -> Int
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


get : comparable -> Node comparable v -> Maybe v
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


member : comparable -> Node comparable v -> Bool
member key node =
    case get key node of
        Just _ ->
            True

        Nothing ->
            False



-- zipping


{-| Represents the location of a specific subnode or key within a node.
-}
type UnzippedNode k v
    = NodeLoc (Node k v) (NodeContext k v)
    | KeyLoc ( k, v ) (KeyContext k v)


type NodeContext k v
    = N2A ( k, v ) (Node k v)
    | N2B (Node k v) ( k, v )
    | N3A ( k, v ) (Node k v) ( k, v ) (Node k v)
    | N3B (Node k v) ( k, v ) ( k, v ) (Node k v)
    | N3C (Node k v) ( k, v ) (Node k v) ( k, v )
    | N4A ( k, v ) (Node k v) ( k, v ) (Node k v) ( k, v ) (Node k v)
    | N4B (Node k v) ( k, v ) ( k, v ) (Node k v) ( k, v ) (Node k v)
    | N4C (Node k v) ( k, v ) (Node k v) ( k, v ) ( k, v ) (Node k v)
    | N4D (Node k v) ( k, v ) (Node k v) ( k, v ) (Node k v) ( k, v )


type KeyContext k v
    = N2P1 (Node k v) (Node k v)
    | N3P1 (Node k v) (Node k v) ( k, v ) (Node k v)
    | N3P2 (Node k v) ( k, v ) (Node k v) (Node k v)
    | N4P1 (Node k v) (Node k v) ( k, v ) (Node k v) ( k, v ) (Node k v)
    | N4P2 (Node k v) ( k, v ) (Node k v) (Node k v) ( k, v ) (Node k v)
    | N4P3 (Node k v) ( k, v ) (Node k v) ( k, v ) (Node k v) (Node k v)


seek : comparable -> Node comparable v -> Maybe (UnzippedNode comparable v)
seek key node =
    case node of
        Leaf ->
            Nothing

        N2 a (( k1, _ ) as p1) b ->
            Just
                (if key < k1 then
                    NodeLoc a (N2A p1 b)
                 else if key > k1 then
                    NodeLoc b (N2B a p1)
                 else
                    KeyLoc p1 (N2P1 a b)
                )

        N3 a (( k1, _ ) as p1) b (( k2, _ ) as p2) c ->
            Just
                (if key < k2 then
                    if key < k1 then
                        NodeLoc a (N3A p1 b p2 c)
                    else if key > k1 then
                        NodeLoc b (N3B a p1 p2 c)
                    else
                        KeyLoc p1 (N3P1 a b p2 c)
                 else if key > k2 then
                    NodeLoc c (N3C a p1 b p2)
                 else
                    KeyLoc p2 (N3P2 a p1 b c)
                )

        N4 a (( k1, _ ) as p1) b (( k2, _ ) as p2) c (( k3, _ ) as p3) d ->
            Just
                (if key < k2 then
                    if key < k1 then
                        NodeLoc a (N4A p1 b p2 c p3 d)
                    else if key > k1 then
                        NodeLoc b (N4B a p1 p2 c p3 d)
                    else
                        KeyLoc p1 (N4P1 a b p2 c p3 d)
                 else if key > k2 then
                    if key < k3 then
                        NodeLoc c (N4C a p1 b p2 p3 d)
                    else if key > k3 then
                        NodeLoc d (N4D a p1 b p2 c p3)
                    else
                        KeyLoc p3 (N4P3 a p1 b p2 c d)
                 else
                    KeyLoc p2 (N4P2 a p1 b c p3 d)
                )


zipNodeLoc : Node k v -> NodeContext k v -> Node k v
zipNodeLoc node nodeContext =
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


zipKeyLoc : ( k, v ) -> KeyContext k v -> Node k v
zipKeyLoc pair keyContext =
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


{-| Represents the possible outcomes of modifying a node.
-}
type NodeResult k v
    = Balanced (Node k v)
    | Split (Node k v) ( k, v ) (Node k v)
    | Underfull (Node k v)


{-| This is the only function that changes the depth of the tree; it's to be
used only at the root.
-}
fromNodeResult : NodeResult k v -> Node k v
fromNodeResult nodeResult =
    case nodeResult of
        Balanced n ->
            -- maintains depth
            n

        Split l p r ->
            -- increases depth
            N2 l p r

        Underfull n ->
            -- decreases depth
            n


type RebalanceResult k v
    = Rotated (Node k v) ( k, v ) (Node k v)
    | Merged (Node k v)


rotateLeft : Node k v -> ( k, v ) -> Node k v -> RebalanceResult k v
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


rotateRight : Node k v -> ( k, v ) -> Node k v -> RebalanceResult k v
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


zipNodeResult : NodeContext comparable v -> NodeResult comparable v -> NodeResult comparable v
zipNodeResult nodeContext nodeResult =
    case nodeResult of
        Balanced node ->
            Balanced (zipNodeLoc node nodeContext)

        Split left pair right ->
            case nodeContext of
                N2A p1 b ->
                    Balanced (N3 left pair right p1 b)

                N2B a p1 ->
                    Balanced (N3 a p1 left pair right)

                N3A p1 b p2 c ->
                    Balanced (N4 left pair right p1 b p2 c)

                N3B a p1 p2 c ->
                    Balanced (N4 a p1 left pair right p2 c)

                N3C a p1 b p2 ->
                    Balanced (N4 a p1 b p2 left pair right)

                N4A p1 b p2 c p3 d ->
                    Split (N3 left pair right p1 b) p2 (N2 c p3 d)

                N4B a p1 p2 c p3 d ->
                    Split (N3 a p1 left pair right) p2 (N2 c p3 d)

                N4C a p1 b p2 p3 d ->
                    Split (N3 a p1 b p2 left) pair (N2 right p3 d)

                N4D a p1 b p2 c p3 ->
                    Split (N3 a p1 b p2 c) p3 (N2 left pair right)

        Underfull subnode ->
            case nodeContext of
                N2A p1 b ->
                    -- leftmost
                    case rotateLeft subnode p1 b of
                        Rotated newA newP1 newB ->
                            Balanced (N2 newA newP1 newB)

                        Merged ab ->
                            Underfull ab

                N2B a p1 ->
                    -- rightmost
                    case rotateRight a p1 subnode of
                        Rotated newA newP1 newB ->
                            Balanced (N2 newA newP1 newB)

                        Merged ab ->
                            Underfull ab

                N3A p1 b p2 c ->
                    -- leftmost
                    case rotateLeft subnode p1 b of
                        Rotated newA newP1 newB ->
                            Balanced (N3 newA newP1 newB p2 c)

                        Merged ab ->
                            Balanced (N2 ab p2 c)

                N3B a p1 p2 c ->
                    -- middle
                    case rotateLeft subnode p2 c of
                        Rotated newB newP2 newC ->
                            Balanced (N3 a p1 newB newP2 newC)

                        _ ->
                            case rotateRight a p1 subnode of
                                Rotated newA newP1 newB ->
                                    Balanced (N3 newA newP1 newB p2 c)

                                Merged ab ->
                                    Balanced (N2 ab p2 c)

                N3C a p1 b p2 ->
                    -- rightmost
                    case rotateRight b p2 subnode of
                        Rotated newB newP2 newC ->
                            Balanced (N3 a p1 newB newP2 newC)

                        Merged bc ->
                            Balanced (N2 a p1 bc)

                N4A p1 b p2 c p3 d ->
                    -- leftmost
                    case rotateLeft subnode p1 b of
                        Rotated newA newP1 newB ->
                            Balanced (N4 newA newP1 newB p2 c p3 d)

                        Merged ab ->
                            Balanced (N3 ab p2 c p3 d)

                N4B a p1 p2 c p3 d ->
                    -- middle
                    case rotateLeft subnode p2 c of
                        Rotated newB newP2 newC ->
                            Balanced (N4 a p1 newB newP2 newC p3 d)

                        _ ->
                            case rotateRight a p1 subnode of
                                Rotated newA newP1 newB ->
                                    Balanced (N4 newA newP1 newB p2 c p3 d)

                                Merged ab ->
                                    Balanced (N3 ab p2 c p3 d)

                N4C a p1 b p2 p3 d ->
                    -- middle
                    case rotateLeft subnode p3 d of
                        Rotated newC newP3 newD ->
                            Balanced (N4 a p1 b p2 newC newP3 newD)

                        _ ->
                            case rotateRight b p2 subnode of
                                Rotated newB newP2 newC ->
                                    Balanced (N4 a p1 newB newP2 newC p3 d)

                                Merged bc ->
                                    Balanced (N3 a p1 bc p3 d)

                N4D a p1 b p2 c p3 ->
                    -- rightmost
                    case rotateRight c p3 subnode of
                        Rotated newC newP3 newD ->
                            Balanced (N4 a p1 b p2 newC newP3 newD)

                        Merged cd ->
                            Balanced (N3 a p1 b p2 cd)


zipWithoutKey : KeyContext comparable v -> NodeResult comparable v
zipWithoutKey keyContext =
    case keyContext of
        N2P1 a b ->
            case findReplacementKey a b of
                Rotated newA newP1 newB ->
                    Balanced (N2 newA newP1 newB)

                Merged ab ->
                    Underfull ab

        N3P1 a b p2 c ->
            case findReplacementKey a b of
                Rotated newA newP1 newB ->
                    Balanced (N3 newA newP1 newB p2 c)

                Merged ab ->
                    Balanced (N2 ab p2 c)

        N3P2 a p1 b c ->
            case findReplacementKey b c of
                Rotated newB newP2 newC ->
                    Balanced (N3 a p1 newB newP2 newC)

                Merged bc ->
                    Balanced (N2 a p1 bc)

        N4P1 a b p2 c p3 d ->
            case findReplacementKey a b of
                Rotated newA newP1 newB ->
                    Balanced (N4 newA newP1 newB p2 c p3 d)

                Merged ab ->
                    Balanced (N3 ab p2 c p3 d)

        N4P2 a p1 b c p3 d ->
            case findReplacementKey b c of
                Rotated newB newP2 newC ->
                    Balanced (N4 a p1 newB newP2 newC p3 d)

                Merged bc ->
                    Balanced (N3 a p1 bc p3 d)

        N4P3 a p1 b p2 c d ->
            case findReplacementKey c d of
                Rotated newC newP3 newD ->
                    Balanced (N4 a p1 b p2 newC newP3 newD)

                Merged cd ->
                    Balanced (N3 a p1 b p2 cd)


findReplacementKey : Node comparable v -> Node comparable v -> RebalanceResult comparable v
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
                            case removeHelp key right of
                                Balanced newRight ->
                                    Rotated left pair newRight

                                Underfull rightSubnode ->
                                    rotateRight left pair rightSubnode

                                Split _ _ _ ->
                                    Debug.crash "remove can't result in a split"

                        Nothing ->
                            Merged Leaf


borrowSmallest : Node k v -> Maybe ( ( k, v ), Node k v )
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


borrowLargest : Node k v -> Maybe ( Node k v, ( k, v ) )
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


getSmallest : Maybe ( k, v ) -> Node k v -> Maybe ( k, v )
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



-- insert


insert : comparable -> v -> Node comparable v -> Node comparable v
insert key val =
    insertHelp ( key, val ) >> fromNodeResult


insertHelp : ( comparable, v ) -> Node comparable v -> NodeResult comparable v
insertHelp (( key, _ ) as pair) node =
    case seek key node of
        Nothing ->
            Split Leaf pair Leaf

        Just loc ->
            case loc of
                NodeLoc subnode nodeContext ->
                    zipNodeResult nodeContext (insertHelp pair subnode)

                KeyLoc _ keyContext ->
                    Balanced (zipKeyLoc pair keyContext)



-- remove


remove : comparable -> Node comparable v -> Node comparable v
remove key =
    removeHelp key >> fromNodeResult


removeHelp : comparable -> Node comparable v -> NodeResult comparable v
removeHelp key node =
    case seek key node of
        Nothing ->
            Balanced node

        Just loc ->
            case loc of
                NodeLoc subnode nodeContext ->
                    zipNodeResult nodeContext (removeHelp key subnode)

                KeyLoc _ keyContext ->
                    zipWithoutKey keyContext



-- update


update : comparable -> (Maybe v -> Maybe v) -> Node comparable v -> Node comparable v
update key f =
    updateHelp key f >> fromNodeResult


updateHelp : comparable -> (Maybe v -> Maybe v) -> Node comparable v -> NodeResult comparable v
updateHelp key f node =
    case seek key node of
        Nothing ->
            case f Nothing of
                Just val ->
                    Split Leaf ( key, val ) Leaf

                Nothing ->
                    Balanced node

        Just loc ->
            case loc of
                NodeLoc subnode nodeContext ->
                    zipNodeResult nodeContext (updateHelp key f subnode)

                KeyLoc ( _, val ) keyContext ->
                    case f (Just val) of
                        Just newVal ->
                            Balanced (zipKeyLoc ( key, newVal ) keyContext)

                        Nothing ->
                            zipWithoutKey keyContext



-- map


map : (k -> a -> b) -> Node k a -> Node k b
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


foldl : (k -> v -> a -> a) -> a -> Node k v -> a
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


foldr : (k -> v -> a -> a) -> a -> Node k v -> a
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
foldr_ : (( k, v ) -> a -> a) -> a -> Node k v -> a
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


filter : (k -> v -> Bool) -> Node k v -> Node k v
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


partition : (k -> v -> Bool) -> Node k v -> ( Node k v, Node k v )
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



-- list


keys : Node k v -> List k
keys =
    foldr (\k _ -> (::) k) []


values : Node k v -> List v
values =
    foldr (\_ v -> (::) v) []


toList : Node k v -> List ( k, v )
toList =
    foldr_ (::) []


{-| Builds tree from the bottom up
-}
fromList : List ( comparable, v ) -> Node comparable v
fromList =
    List.sortBy Tuple.first >> deduplicate >> fromSortedList False


{-| Strips consecutive duplicates, where last duplicate wins; reverses list
-}
deduplicate : List ( comparable, v ) -> List ( comparable, v )
deduplicate list =
    case list of
        x :: list ->
            deduplicateHelp [] x list

        [] ->
            []


deduplicateHelp : List ( comparable, v ) -> ( comparable, v ) -> List ( comparable, v ) -> List ( comparable, v )
deduplicateHelp revList (( key, _ ) as pair) list =
    case list of
        (( nextKey, _ ) as nextPair) :: rest ->
            if key == nextKey then
                deduplicateHelp (revList) nextPair rest
            else
                deduplicateHelp (pair :: revList) nextPair rest

        [] ->
            pair :: revList


{-| Expects keys in assoc list to be sorted and distinct
-}
fromSortedList : Bool -> List ( k, v ) -> Node k v
fromSortedList isAsc list =
    case list of
        [] ->
            Leaf

        x :: rest ->
            if isAsc then
                pairsToNodeListAsc [] x rest |> fromNodeList True
            else
                pairsToNodeListDesc [] x rest |> fromNodeList False


type alias NodeList k v =
    ( Node k v, List ( ( k, v ), Node k v ) )


pairsToNodeListAsc : List ( ( k, v ), Node k v ) -> ( k, v ) -> List ( k, v ) -> NodeList k v
pairsToNodeListAsc revList a list =
    case list of
        [] ->
            ( N2 Leaf a Leaf, revList )

        b :: [] ->
            ( N3 Leaf a Leaf b Leaf, revList )

        b :: c :: [] ->
            ( N4 Leaf a Leaf b Leaf c Leaf, revList )

        b :: c :: d :: [] ->
            ( N2 Leaf d Leaf, ( c, N3 Leaf a Leaf b Leaf ) :: revList )

        b :: c :: d :: e :: rest ->
            pairsToNodeListAsc (( d, N4 Leaf a Leaf b Leaf c Leaf ) :: revList) e rest


pairsToNodeListDesc : List ( ( k, v ), Node k v ) -> ( k, v ) -> List ( k, v ) -> NodeList k v
pairsToNodeListDesc revList a list =
    case list of
        [] ->
            ( N2 Leaf a Leaf, revList )

        b :: [] ->
            ( N3 Leaf b Leaf a Leaf, revList )

        b :: c :: [] ->
            ( N4 Leaf c Leaf b Leaf a Leaf, revList )

        b :: c :: d :: [] ->
            ( N2 Leaf d Leaf, ( c, N3 Leaf b Leaf a Leaf ) :: revList )

        b :: c :: d :: e :: rest ->
            pairsToNodeListDesc (( d, N4 Leaf c Leaf b Leaf a Leaf ) :: revList) e rest


fromNodeList : Bool -> NodeList k v -> Node k v
fromNodeList isReversed nodeList =
    case nodeList of
        ( node, [] ) ->
            node

        ( a, ( p1, b ) :: list ) ->
            accumulateNodeList isReversed [] a p1 b list |> fromNodeList (not isReversed)


{-| Reverses list
-}
accumulateNodeList : Bool -> List ( ( k, v ), Node k v ) -> Node k v -> ( k, v ) -> Node k v -> List ( ( k, v ), Node k v ) -> NodeList k v
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
