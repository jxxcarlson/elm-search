module Search exposing (search, SearchConfig(..))

import Parse exposing(parse)
import APITypes exposing(Term(..), Datum)
import Time


type SearchConfig = CaseSensitive | NotCaseSensitive


search : SearchConfig -> String -> List (Datum data ) -> List (Datum data )
search config queryString dataList = 
  case parse queryString of 
    Ok term -> searchWithTerm config term dataList 
    Err _ -> dataList

searchWithTerm : SearchConfig -> Term -> List (Datum data ) -> List (Datum data )
searchWithTerm config term dataList = 
 case config of
    CaseSensitive ->  List.filter (queryCaseSenstiive term) dataList
    NotCaseSensitive ->  List.filter (queryNotCaseSenstiive term) dataList


queryCaseSenstiive : Term -> Datum data -> Bool
queryCaseSenstiive  term =
  case term of 
    Word str -> (\datum -> datum.content == str)
    NotWord str -> (\datum -> datum.content /= str)
    Conjunction terms -> (\datum -> List.foldl (\term_ acc -> matchCaseSenstive term_  datum.content && acc) True terms)
    BeforeDateTime dt -> (\datum -> (Time.toMillis Time.utc datum.dateTime)  < (Time.toMillis Time.utc dt))
    AfterDateTime dt -> (\datum -> (Time.toMillis Time.utc datum.dateTime)  > (Time.toMillis Time.utc dt))

    

queryNotCaseSenstiive : Term -> Datum data -> Bool
queryNotCaseSenstiive  term =
  case term of 
    Word str -> (\datum -> datum.content == str)
    NotWord str -> (\datum -> datum.content /= str)
    Conjunction terms -> (\datum -> List.foldl (\term_ acc -> matchNotCaseSenstive term_  datum.content && acc) True terms)
    BeforeDateTime dt -> (\datum -> (Time.toMillis Time.utc datum.dateTime)  <= (Time.toMillis Time.utc dt))
    AfterDateTime dt -> (\datum -> (Time.toMillis Time.utc datum.dateTime)  >= (Time.toMillis Time.utc dt))



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