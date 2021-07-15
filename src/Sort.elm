module Sort exposing (sort, SortParam(..), Direction(..))

{-|

@docs sort, SortParam, Direction

-}

import APITypes exposing (Datum)
import Random
import Random.List
import Time


{-| -}
type SortParam
    = Alpha Direction
    | DateTime Direction
    | Random Random.Seed



--| Random Int


{-| -}
type Direction
    = Increasing
    | Decreasing


{-| -}
type Filter
    = DateAfter String
    | DateBefore String


{-| -}
sort : SortParam -> List (Datum data) -> List (Datum data)
sort param dataList =
    case param of
        Alpha Increasing ->
            List.sortWith (\a b -> compare a.content b.content) dataList

        Alpha Decreasing ->
            List.sortWith (\a b -> compare b.content a.content) dataList

        DateTime Increasing ->
            List.sortWith (\a b -> compare (Time.posixToMillis a.dateTime) (Time.posixToMillis b.dateTime)) dataList

        DateTime Decreasing ->
            List.sortWith (\a b -> compare (Time.posixToMillis b.dateTime) (Time.posixToMillis a.dateTime)) dataList

        Random seed ->
            Random.step (Random.List.shuffle dataList) seed |> Tuple.first
