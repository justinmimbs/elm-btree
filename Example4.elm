module Example4 exposing (..)

import Char
import Dict.BTree4 as Dict2 exposing (Node(..))


letters =
    "CSDTAMPIBWNGURKEHOLJYQZFXV"


ex1 =
    String.toList letters
        |> List.map (\c -> ( c, Char.toLower c ))
        |> Dict2.fromList


valuesFromEx1 =
    String.toList ("0123456789" ++ letters ++ " ")
        |> List.filterMap (\c -> Dict2.get c ex1)
        |> String.fromList


tests =
    [ valuesFromEx1 == String.toLower letters
    ]
