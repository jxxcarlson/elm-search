module Parse exposing (dateTimeFromDateString, beforeDate, afterDate, posixFromDateString, parse)

import APITypes exposing (..)
import DateTime
import Library.DateTime
import Library.ParserTools exposing (manySeparatedBy, text)
import Parser exposing ((|.), (|=), Parser, run)
import Maybe.Extra
import Time

parse input =
    Parser.run conjunction input


posixFromDateString dateString =
    dateString |> dateTimeFromDateString |> Maybe.map DateTime.toPosix 



-- |> Maybe.map DateTime.toPosix


{-|

> dateTimeFromDateString "6/1/2000"

    Just (Just (DateTime { date = Date { day = Day 1, month = Jun, year = Year 2000 }, time = Time { hours = Hour 23, milliseconds = Millisecond 0, minutes = Minute 59, seconds = Second 59 } }))

-}
dateTimeFromDateString dateString =
    Maybe.map2 DateTime.fromRawParts (Library.DateTime.rawDateFromString dateString) (Just Library.DateTime.midnight)
      |> Maybe.Extra.join


conjunction : Parser Term
conjunction =
    manySeparatedBy Parser.spaces term |> Parser.map Conjunction



{-|
  > run beforeDate "before:6/1/2001"
    Ok (BeforeDateTime (Posix 991439999000))
-}
beforeDate : Parser Term
beforeDate = 
  Parser.succeed(\s -> posixFromDateString s.content |> Maybe.withDefault (Time.millisToPosix 0) |> BeforeDateTime)
    |. Parser.symbol "before:"
    |= text (\c -> c /= ' ') (\c -> c /= ' ')


{-| 
  > run afterDate "after:6/1/2001"
    Ok (AfterDateTime (Posix 991439999000))
-}
afterDate : Parser  Term 
afterDate = 
  Parser.succeed(\s -> posixFromDateString s.content |> Maybe.withDefault (Time.millisToPosix 0) |> AfterDateTime)
    |. Parser.symbol "after:"
    |= text (\c -> c /= ' ') (\c -> c /= ' ')   
{-|

    > run word "foo"
      Ok (Word "foo") : Result (List DeadEnd) API.Term

    > run word "-bar"
      Ok (NotWord "bar")

-}
term : Parser Term
term =
    Parser.oneOf [ negativeWord, positiveWord, beforeDate, afterDate ]


{-|

    > run positiveWord "foo bar"
      Ok (Word "foo") : Result (List DeadEnd) API.Term

-}
positiveWord : Parser Term
positiveWord =
    text (\c -> c /= ' ') (\c -> c /= ' ') |> Parser.map .content |> Parser.map Word


{-|

> run negativeWord "-foo"

     Ok (NotWord "foo") : Result (List DeadEnd) API.Term

-}
negativeWord : Parser Term
negativeWord =
    (Parser.succeed (\r -> r.content)
        |. Parser.symbol "-"
        |= text (\c -> c /= ' ') (\c -> c /= ' ')
    )
        |> Parser.map NotWord
