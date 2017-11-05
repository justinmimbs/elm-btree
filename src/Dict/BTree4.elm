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
    | K1 (Node k v) ( k, v ) (Node k v)
    | K2 (Node k v) ( k, v ) (Node k v) ( k, v ) (Node k v)
    | K3 (Node k v) ( k, v ) (Node k v) ( k, v ) (Node k v) ( k, v ) (Node k v)


empty : Node k v
empty =
    Leaf


get : comparable -> Node comparable v -> Maybe v
get key node =
    case node of
        Leaf ->
            Nothing

        K1 a ( k1, v1 ) b ->
            if key < k1 then
                get key a
            else if key > k1 then
                get key b
            else
                Just v1

        K2 a ( k1, v1 ) b ( k2, v2 ) c ->
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

        K3 a ( k1, v1 ) b ( k2, v2 ) c ( k3, v3 ) d ->
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
    = K1A ( k, v ) (Node k v)
    | K1B (Node k v) ( k, v )
    | K2A ( k, v ) (Node k v) ( k, v ) (Node k v)
    | K2B (Node k v) ( k, v ) ( k, v ) (Node k v)
    | K2C (Node k v) ( k, v ) (Node k v) ( k, v )
    | K3A ( k, v ) (Node k v) ( k, v ) (Node k v) ( k, v ) (Node k v)
    | K3B (Node k v) ( k, v ) ( k, v ) (Node k v) ( k, v ) (Node k v)
    | K3C (Node k v) ( k, v ) (Node k v) ( k, v ) ( k, v ) (Node k v)
    | K3D (Node k v) ( k, v ) (Node k v) ( k, v ) (Node k v) ( k, v )


type KeyContext k v
    = K1P1 (Node k v) (Node k v)
    | K2P1 (Node k v) (Node k v) ( k, v ) (Node k v)
    | K2P2 (Node k v) ( k, v ) (Node k v) (Node k v)
    | K3P1 (Node k v) (Node k v) ( k, v ) (Node k v) ( k, v ) (Node k v)
    | K3P2 (Node k v) ( k, v ) (Node k v) (Node k v) ( k, v ) (Node k v)
    | K3P3 (Node k v) ( k, v ) (Node k v) ( k, v ) (Node k v) (Node k v)


seek : comparable -> Node comparable v -> Maybe (UnzippedNode comparable v)
seek key node =
    case node of
        Leaf ->
            Nothing

        K1 a (( k1, _ ) as p1) b ->
            Just
                (if key < k1 then
                    NodeLoc a (K1A p1 b)
                 else if key > k1 then
                    NodeLoc b (K1B a p1)
                 else
                    KeyLoc p1 (K1P1 a b)
                )

        K2 a (( k1, _ ) as p1) b (( k2, _ ) as p2) c ->
            Just
                (if key < k2 then
                    if key < k1 then
                        NodeLoc a (K2A p1 b p2 c)
                    else if key > k1 then
                        NodeLoc b (K2B a p1 p2 c)
                    else
                        KeyLoc p1 (K2P1 a b p2 c)
                 else if key > k2 then
                    NodeLoc c (K2C a p1 b p2)
                 else
                    KeyLoc p2 (K2P2 a p1 b c)
                )

        K3 a (( k1, _ ) as p1) b (( k2, _ ) as p2) c (( k3, _ ) as p3) d ->
            Just
                (if key < k2 then
                    if key < k1 then
                        NodeLoc a (K3A p1 b p2 c p3 d)
                    else if key > k1 then
                        NodeLoc b (K3B a p1 p2 c p3 d)
                    else
                        KeyLoc p1 (K3P1 a b p2 c p3 d)
                 else if key > k2 then
                    if key < k3 then
                        NodeLoc c (K3C a p1 b p2 p3 d)
                    else if key > k3 then
                        NodeLoc d (K3D a p1 b p2 c p3)
                    else
                        KeyLoc p3 (K3P3 a p1 b p2 c d)
                 else
                    KeyLoc p2 (K3P2 a p1 b c p3 d)
                )


zipNodeLoc : Node k v -> NodeContext k v -> Node k v
zipNodeLoc node nodeContext =
    case nodeContext of
        K1A p1 b ->
            K1 node p1 b

        K1B a p1 ->
            K1 a p1 node

        K2A p1 b p2 c ->
            K2 node p1 b p2 c

        K2B a p1 p2 c ->
            K2 a p1 node p2 c

        K2C a p1 b p2 ->
            K2 a p1 b p2 node

        K3A p1 b p2 c p3 d ->
            K3 node p1 b p2 c p3 d

        K3B a p1 p2 c p3 d ->
            K3 a p1 node p2 c p3 d

        K3C a p1 b p2 p3 d ->
            K3 a p1 b p2 node p3 d

        K3D a p1 b p2 c p3 ->
            K3 a p1 b p2 c p3 node


zipKeyLoc : ( k, v ) -> KeyContext k v -> Node k v
zipKeyLoc pair keyContext =
    case keyContext of
        K1P1 a b ->
            K1 a pair b

        K2P1 a b p2 c ->
            K2 a pair b p2 c

        K2P2 a p1 b c ->
            K2 a p1 b pair c

        K3P1 a b p2 c p3 d ->
            K3 a pair b p2 c p3 d

        K3P2 a p1 b c p3 d ->
            K3 a p1 b pair c p3 d

        K3P3 a p1 b p2 c d ->
            K3 a p1 b p2 c pair d


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
                K1A p1 b ->
                    Balanced (K2 left pair right p1 b)

                K1B a p1 ->
                    Balanced (K2 a p1 left pair right)

                K2A p1 b p2 c ->
                    Balanced (K3 left pair right p1 b p2 c)

                K2B a p1 p2 c ->
                    Balanced (K3 a p1 left pair right p2 c)

                K2C a p1 b p2 ->
                    Balanced (K3 a p1 b p2 left pair right)

                K3A p1 b p2 c p3 d ->
                    Split (K2 left pair right p1 b) p2 (K1 c p3 d)

                K3B a p1 p2 c p3 d ->
                    Split (K2 a p1 left pair right) p2 (K1 c p3 d)

                K3C a p1 b p2 p3 d ->
                    Split (K2 a p1 b p2 left) pair (K1 right p3 d)

                K3D a p1 b p2 c p3 ->
                    Split (K2 a p1 b p2 c) p3 (K1 left pair right)

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
            K1 l p r

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

        K1 a ( k1, v1 ) b ->
            foldl f (f k1 v1 (foldl f result a)) b

        K2 a ( k1, v1 ) b ( k2, v2 ) c ->
            foldl f (f k2 v2 (foldl f (f k1 v1 (foldl f result a)) b)) c

        K3 a ( k1, v1 ) b ( k2, v2 ) c ( k3, v3 ) d ->
            foldl f (f k3 v3 (foldl f (f k2 v2 (foldl f (f k1 v1 (foldl f result a)) b)) c)) d


foldr : (k -> v -> a -> a) -> a -> Node k v -> a
foldr f result node =
    case node of
        Leaf ->
            result

        K1 a ( k1, v1 ) b ->
            foldr f (f k1 v1 (foldr f result b)) a

        K2 a ( k1, v1 ) b ( k2, v2 ) c ->
            foldr f (f k1 v1 (foldr f (f k2 v2 (foldr f result c)) b)) a

        K3 a ( k1, v1 ) b ( k2, v2 ) c ( k3, v3 ) d ->
            foldr f (f k1 v1 (foldr f (f k2 v2 (foldr f (f k3 v3 (foldr f result d)) c)) b)) a



-- list


toList : Node k v -> List ( k, v )
toList =
    foldr (\k v -> (::) ( k, v )) []


{-| build tree from the bottom up
-}
fromList : List ( comparable, v ) -> Node comparable v
fromList =
    List.sortBy Tuple.first >> deduplicate >> nodeListFromRevList >> fromNodeList


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
deduplicateHelp rev (( kx, _ ) as x) list =
    case list of
        (( ky, _ ) as y) :: rest ->
            if kx == ky then
                deduplicateHelp (rev) y rest
            else
                deduplicateHelp (x :: rev) y rest

        [] ->
            x :: rev


type alias NodeList k v =
    ( Node k v, List ( ( k, v ), Node k v ) )


type alias NodeListRev k v =
    ( List ( Node k v, ( k, v ) ), Node k v )


nodeListFromRevList : List ( k, v ) -> NodeList k v
nodeListFromRevList list =
    case list of
        [] ->
            ( Leaf, [] )

        x :: rest ->
            nodeListFromRevListHelp [] x rest


nodeListFromRevListHelp : List ( ( k, v ), Node k v ) -> ( k, v ) -> List ( k, v ) -> NodeList k v
nodeListFromRevListHelp kn a list =
    case list of
        [] ->
            ( K1 Leaf a Leaf, kn )

        b :: [] ->
            ( K2 Leaf b Leaf a Leaf, kn )

        b :: c :: [] ->
            ( K3 Leaf c Leaf b Leaf a Leaf, kn )

        b :: c :: d :: [] ->
            ( K1 Leaf d Leaf, ( c, K2 Leaf b Leaf a Leaf ) :: kn )

        b :: c :: d :: e :: rest ->
            nodeListFromRevListHelp (( d, K3 Leaf c Leaf b Leaf a Leaf ) :: kn) e rest


fromNodeList : NodeList k v -> Node k v
fromNodeList nkn =
    case nkn of
        ( node, [] ) ->
            node

        ( a, ( p1, b ) :: kn ) ->
            case accumulateNodeList [] a p1 b kn of
                ( [], node ) ->
                    node

                ( ( b2, p12 ) :: nk, a2 ) ->
                    accumulateNodeListRev [] a2 p12 b2 nk |> fromNodeList


accumulateNodeList : List ( Node k v, ( k, v ) ) -> Node k v -> ( k, v ) -> Node k v -> List ( ( k, v ), Node k v ) -> NodeListRev k v
accumulateNodeList acc a p1 b kn =
    case kn of
        [] ->
            ( acc, K1 a p1 b )

        ( p2, c ) :: [] ->
            ( acc, K2 a p1 b p2 c )

        ( p2, c ) :: ( p3, d ) :: [] ->
            ( acc, K3 a p1 b p2 c p3 d )

        ( p2, c ) :: ( p3, d ) :: ( p4, e ) :: [] ->
            ( ( K2 a p1 b p2 c, p3 ) :: acc, K1 d p4 e )

        ( p2, c ) :: ( p3, d ) :: ( p4, e ) :: ( p5, f ) :: kn2 ->
            accumulateNodeList (( K3 a p1 b p2 c p3 d, p4 ) :: acc) e p5 f kn2


accumulateNodeListRev : List ( ( k, v ), Node k v ) -> Node k v -> ( k, v ) -> Node k v -> List ( Node k v, ( k, v ) ) -> NodeList k v
accumulateNodeListRev acc a p1 b nk =
    case nk of
        [] ->
            ( K1 b p1 a, acc )

        ( c, p2 ) :: [] ->
            ( K2 c p2 b p1 a, acc )

        ( c, p2 ) :: ( d, p3 ) :: [] ->
            ( K3 d p3 c p2 b p1 a, acc )

        ( c, p2 ) :: ( d, p3 ) :: ( e, p4 ) :: [] ->
            ( K1 e p4 d, ( p3, K2 c p2 b p1 a ) :: acc )

        ( c, p2 ) :: ( d, p3 ) :: ( e, p4 ) :: ( f, p5 ) :: nk2 ->
            accumulateNodeListRev (( p4, K3 d p3 c p2 b p1 a ) :: acc) e p5 f nk2
