module Dict.BTree4
    exposing
        ( Node(..)
        , empty
        , get
        , insert
        , foldl
        , foldr
        , fromList
        , toList
        )

{-| Order-4 B-tree (i.e. per node: max subnodes == 4, max keys == 3, min keys == 1)
-}


type Node k v
    = Leaf
    | N2 (Node k v) ( k, v ) (Node k v)
    | N3 (Node k v) ( k, v ) (Node k v) ( k, v ) (Node k v)
    | N4 (Node k v) ( k, v ) (Node k v) ( k, v ) (Node k v) ( k, v ) (Node k v)


empty : Node k v
empty =
    Leaf


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


type NodeResult k v
    = Balanced (Node k v)
    | Split (Node k v) ( k, v ) (Node k v)
    | Underfull (Node k v)


zipNodeResult : NodeContext k v -> NodeResult k v -> NodeResult k v
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

        Underfull n ->
            Debug.crash "underfull"


nodeResultToRoot : NodeResult k v -> Node k v
nodeResultToRoot nodeResult =
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


insert : comparable -> v -> Node comparable v -> Node comparable v
insert key val =
    insertHelp ( key, val ) >> nodeResultToRoot


insertHelp : ( comparable, v ) -> Node comparable v -> NodeResult comparable v
insertHelp (( key, _ ) as pair) node =
    case seek key node of
        Nothing ->
            Split Leaf pair Leaf

        Just (NodeLoc subnode nodeContext) ->
            zipNodeResult nodeContext (insertHelp pair subnode)

        Just (KeyLoc _ keyContext) ->
            Balanced (zipKeyLoc pair keyContext)



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



-- list


toList : Node k v -> List ( k, v )
toList =
    foldr (\k v -> (::) ( k, v )) []


{-| build tree from the bottom up
-}
fromList : List ( comparable, v ) -> Node comparable v
fromList =
    List.sortBy Tuple.first >> deduplicate >> pairsToNodeList >> fromNodeList False


{-| reverses list, last duplicate wins
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


type alias NodeList k v =
    ( Node k v, List ( ( k, v ), Node k v ) )


pairsToNodeList : List ( k, v ) -> NodeList k v
pairsToNodeList list =
    case list of
        [] ->
            ( Leaf, [] )

        x :: rest ->
            pairsToNodeListHelp [] x rest


pairsToNodeListHelp : List ( ( k, v ), Node k v ) -> ( k, v ) -> List ( k, v ) -> NodeList k v
pairsToNodeListHelp revList a list =
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
            pairsToNodeListHelp (( d, N4 Leaf c Leaf b Leaf a Leaf ) :: revList) e rest


fromNodeList : Bool -> NodeList k v -> Node k v
fromNodeList isReversed nodeList =
    case nodeList of
        ( node, [] ) ->
            node

        ( a, ( p1, b ) :: list ) ->
            accumulateNodeList isReversed [] a p1 b list |> fromNodeList (not isReversed)


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
