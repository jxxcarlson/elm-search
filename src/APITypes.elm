module APITypes exposing (Term(..), Datum)

{-|

@docs Term, Datum

-}

import Time


{-| -}
type alias Datum data =
    { data | content : String, dateTime : Time.Posix }


{-| -}
type Term
    = Word String
    | NotWord String
    | Conjunction (List Term)
    | BeforeDateTime Time.Posix
    | AfterDateTime Time.Posix
