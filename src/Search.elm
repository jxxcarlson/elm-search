module Search exposing (..)

{-|

@docs search, SearchConfig

-}

{- (search, SearchConfig(..)) -}

import APITypes exposing (Datum, Term(..))
import Parse exposing (parse)
import Time


{-| -}
type SearchConfig
    = CaseSensitive
    | NotCaseSensitive


{-| -}
search : SearchConfig -> String -> List (Datum data) -> List (Datum data)
search config queryString dataList =
    let
        _ =
            Debug.log "QUERY" (parse queryString)
    in
    case parse queryString of
        Ok term ->
            searchWithTerm config term dataList

        Err _ ->
            dataList


searchWithTerm : SearchConfig -> Term -> List (Datum data) -> List (Datum data)
searchWithTerm config term dataList =
    List.filter (query config term) dataList


query : SearchConfig -> Term -> Datum data -> Bool
query config term =
    case term of
        Word str ->
            case config of
                CaseSensitive ->
                    \datum -> datum.content == str

                NotCaseSensitive ->
                    \datum -> String.toLower datum.content == String.toLower str

        NotWord str ->
            case config of
                CaseSensitive ->
                    \datum -> datum.content /= str

                NotCaseSensitive ->
                    \datum -> String.toLower datum.content /= String.toLower str

        Conjunction terms ->
            \datum -> List.foldl (\term_ acc -> match config term_ datum && acc) True terms

        BeforeDateTime dt ->
            \datum -> posixLTEForDatum datum dt

        AfterDateTime dt ->
            \datum -> posixGTEForDatum datum dt

        Range dt1 dt2 ->
            --\datum -> posixGTEForDatum datum dt1 && posixLTEForDatum datum dt2
            --\datum -> posixLTEForDatum datum dt2
            \datum -> posixLTEForDatum datum dt2


posixGTE a b =
    Time.posixToMillis a >= Time.posixToMillis b


posixGTEForDatum : Datum data -> Time.Posix -> Bool
posixGTEForDatum a b =
    Time.posixToMillis a.dateTime >= Time.posixToMillis b


posixLTE a b =
    Time.posixToMillis a <= Time.posixToMillis b


posixLTEForDatum : { x | dateTime : Time.Posix } -> Time.Posix -> Bool
posixLTEForDatum a b =
    Time.posixToMillis a.dateTime <= Time.posixToMillis b


posixZero =
    Time.millisToPosix 0


matchCaseSenstive : Term -> String -> Bool
matchCaseSenstive term str =
    case term of
        Word w ->
            String.contains w str

        NotWord w ->
            not (String.contains w str)

        _ ->
            False


match : SearchConfig -> Term -> Datum data -> Bool
match config term datum =
    case term of
        Word w ->
            case config of
                CaseSensitive ->
                    String.contains w datum.content

                NotCaseSensitive ->
                    String.contains (String.toLower w) (String.toLower datum.content)

        NotWord w ->
            case config of
                CaseSensitive ->
                    not (String.contains w datum.content)

                NotCaseSensitive ->
                    not (String.contains (String.toLower w) (String.toLower datum.content))

        BeforeDateTime dt ->
            posixLTEForDatum datum dt

        AfterDateTime dt ->
            posixGTEForDatum datum dt

        _ ->
            False
