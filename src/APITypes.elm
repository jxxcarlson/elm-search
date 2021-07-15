module APITypes exposing (Datum, Term(..), nullTerm)

import Time


type alias Datum data =
    { data | content : String, dateTime : Time.Posix} -- dateTime = milliseconds


type Term
    = Word String
    | NotWord String
    | Conjunction (List Term)
    | BeforeDateTime Time.Posix
    | AfterDateTime Time.Posix


nullTerm = Word "null"

-- type alias Configuration= {
--       sort : List Sort
--     , filter : List Filter
--   }
