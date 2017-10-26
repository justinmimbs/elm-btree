module BTree exposing (Node, empty, get, insert)


type Node k v
    = Leaf (List ( k, v ))
    | NonLeaf (Node k v) (List ( k, v, Node k v ))


{-| maxKeys == maxNodes - 1
-}
maxKeys : Int
maxKeys =
    3


empty : Node comparable v
empty =
    Leaf []


get : comparable -> Node comparable v -> Maybe v
get key node =
    case node of
        Leaf list ->
            getFromLeaf key list

        NonLeaf head tail ->
            getFromNonLeaf key head tail


{-| assumes ordered list
-}
getFromLeaf : comparable -> List ( comparable, v ) -> Maybe v
getFromLeaf key list =
    case list of
        ( k, v ) :: rest ->
            if key > k then
                getFromLeaf key rest
            else if key == k then
                Just v
            else
                Nothing

        _ ->
            Nothing


{-| assumes ordered list
-}
getFromNonLeaf : comparable -> Node comparable v -> List ( comparable, v, Node comparable v ) -> Maybe v
getFromNonLeaf key head tail =
    case tail of
        ( k, v, head2 ) :: tail2 ->
            if key > k then
                getFromNonLeaf key head2 tail2
            else if key == k then
                Just v
            else
                get key head

        [] ->
            get key head


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
                    splitLeaf list2

        NonLeaf head tail ->
            let
                ( head2, tail2 ) =
                    insertIntoNonLeaf key val head tail
            in
                if List.length tail2 <= maxKeys then
                    NoSplit (NonLeaf head2 tail2)
                else
                    splitNonLeaf head2 tail2


splitLeaf : List ( comparable, v ) -> InsertResult comparable v
splitLeaf list =
    case split list of
        ( left, ( key, val ) :: right ) ->
            Split (Leaf left) key val (Leaf right)

        _ ->
            NoSplit (Leaf list)


splitNonLeaf : Node comparable v -> List ( comparable, v, Node comparable v ) -> InsertResult comparable v
splitNonLeaf head tail =
    case split tail of
        ( leftTail, ( key, val, rightHead ) :: rightTail ) ->
            Split (NonLeaf head leftTail) key val (NonLeaf rightHead rightTail)

        _ ->
            NoSplit (NonLeaf head tail)


{-| assumes ordered list
-}
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


{-| assumes ordered list
-}
insertIntoNonLeaf : comparable -> v -> Node comparable v -> List ( comparable, v, Node comparable v ) -> ( Node comparable v, List ( comparable, v, Node comparable v ) )
insertIntoNonLeaf key val head tail =
    case tail of
        ( k, v, head2 ) :: tail2 ->
            if key > k then
                consNonLeaf head k v (insertIntoNonLeaf key val head2 tail2)
            else if key == k then
                ( head, ( key, val, head2 ) :: tail2 )
            else
                insertIntoNonLeafAtHead key val head tail

        [] ->
            insertIntoNonLeafAtHead key val head []


insertIntoNonLeafAtHead : comparable -> v -> Node comparable v -> List ( comparable, v, Node comparable v ) -> ( Node comparable v, List ( comparable, v, Node comparable v ) )
insertIntoNonLeafAtHead key val target tail =
    case insertHelp key val target of
        NoSplit inserted ->
            ( inserted, tail )

        Split left k v right ->
            ( left, ( k, v, right ) :: tail )


consNonLeaf : Node comparable v -> comparable -> v -> ( Node comparable v, List ( comparable, v, Node comparable v ) ) -> ( Node comparable v, List ( comparable, v, Node comparable v ) )
consNonLeaf n k v ( head, tail ) =
    ( n, ( k, v, head ) :: tail )


{-| Split `list` into two halves, `( a, b )`, such that `a ++ b == list`.

    split [ 1, 2, 3, 4 ] == ( [ 1, 2 ], [ 3, 4 ] )

If the list is odd in length, then the first half will be the shorter of the halves.

    split [ 1, 2, 3 ] == ( [ 1 ], [ 2, 3 ] )

-}
split : List a -> ( List a, List a )
split list =
    splitHelp (List.length list // 2) [] list


splitHelp : Int -> List a -> List a -> ( List a, List a )
splitHelp i rev list =
    if i <= 0 then
        ( List.reverse rev, list )
    else
        case list of
            x :: rest ->
                splitHelp (i - 1) (x :: rev) rest

            _ ->
                ( [], [] )
