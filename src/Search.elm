module Search exposing (..)


{-

(search, SearchConfig(..))

-}


import Parse exposing(parse)
import APITypes exposing(Term(..), Datum)
import Time


type SearchConfig = CaseSensitive | NotCaseSensitive


search : SearchConfig -> String -> List (Datum data ) -> List (Datum data )
search config queryString dataList = 
  let
      -- _ = Debug.log "SEPT" (Parse.posixFromDateString "9/1/2001")
      _ = Debug.log "QUERY" (parse queryString)
  in
  case parse queryString of 
    Ok term -> searchWithTerm config term dataList 
    Err _ -> dataList

searchWithTerm : SearchConfig -> Term -> List (Datum data ) -> List (Datum data )
searchWithTerm config term dataList = 
 case config of
    CaseSensitive ->  List.filter (queryCaseSensitive term) dataList
    NotCaseSensitive ->  List.filter (queryNotCaseSensitive term) dataList


queryCaseSensitive : Term -> Datum data -> Bool
queryCaseSensitive  term =
  case term of 
    Word str -> (\datum -> datum.content == str)
    NotWord str -> (\datum -> datum.content /= str)
    Conjunction terms -> (\datum -> List.foldl (\term_ acc -> matchCaseSenstive term_  datum.content && acc) True terms)
    BeforeDateTime dt -> (\datum ->   posixLTEForDatum datum dt) 
    AfterDateTime dt -> (\datum ->   posixGTEForDatum datum dt) 
    

queryNotCaseSensitive : Term -> Datum data -> Bool
queryNotCaseSensitive  term =
  case term of 
    Word str -> (\datum -> datum.content == String.toLower str)
    NotWord str -> (\datum -> datum.content /= String.toLower str)
    Conjunction terms -> (\datum -> List.foldl (\term_ acc -> matchNotCaseSenstive term_  datum.content && acc) True terms)
    -- Conjunction terms -> (\datum -> List.foldl (\term_ acc -> queryNotCaseSensitive term_  datum && acc) True terms)
    BeforeDateTime dt -> (\datum ->   posixLTEForDatum datum dt) 
    AfterDateTime dt -> (\datum ->   posixGTEForDatum datum dt) 


posixGTE a b = Time.posixToMillis a >= Time.posixToMillis  b

posixGTEForDatum a b = Time.posixToMillis a.dateTime >= Time.posixToMillis  b

posixLTE a b = Time.posixToMillis a <= Time.posixToMillis  b


posixLTEForDatum : { x | dateTime: Time.Posix } -> Time.Posix -> Bool
posixLTEForDatum a b = Time.posixToMillis a.dateTime <= Time.posixToMillis  b

posixZero = Time.millisToPosix 0

matchCaseSenstive : Term -> String -> Bool
matchCaseSenstive  term str = 
   case term of 
     Word w -> String.contains w str
     NotWord w -> not (String.contains w str)
     _ -> False


matchNotCaseSenstive : Term -> String -> Bool
matchNotCaseSenstive  term str = 
   case term of 
     Word w -> String.contains w (String.toLower str)
     NotWord w -> not (String.contains w (String.toLower str))
     _ -> False