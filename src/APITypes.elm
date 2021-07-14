module APITypes exposing (Term(..),  Datum)

import Time

type alias Datum data = { data | content : String, date : Time.Posix }

type Term  = Word String | NotWord String | Conjunction (List Term)

-- type alias Configuration= {
--       sort : List Sort
--     , filter : List Filter
--   }



