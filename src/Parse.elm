module Parse exposing (dateTimeFromDateString, posixFromDateString, parse)

import APITypes exposing (..)
import DateTime
import Library.DateTime
import Library.ParserTools exposing (manySeparatedBy, text)
import Parser exposing ((|.), (|=), Parser, run)
import Maybe.Extra

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
    manySeparatedBy Parser.spaces word |> Parser.map Conjunction


{-|

    > run word "foo"
      Ok (Word "foo") : Result (List DeadEnd) API.Term

    > run word "-bar"
      Ok (NotWord "bar")

-}
word : Parser Term
word =
    Parser.oneOf [ negativeWord, positiveWord ]


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
