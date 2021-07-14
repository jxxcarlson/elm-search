module Filter exposing (..)

import APITypes exposing(Datum)

type SortParam = 
    Alpha Direction
    | Date Direction
    | Random Int

type Direction = Increasing | Decreasing

type Filter = DateAfter String | DateBefore String

sort : SortParam -> List (Datum data) -> List (Datum data)
sort param dataList =
  case param of  
    Alpha Increasing -> List.sortWith (\a b -> compare a.content b.content) dataList
    Alpha Decreasing -> List.sortWith (\a b -> compare b.content a.content) dataList
    _ -> dataList

