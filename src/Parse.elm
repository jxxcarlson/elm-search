module Parse exposing (parse)

import APITypes exposing(..)
import Library.ParserTools exposing(text, manySeparatedBy)
import Parser exposing(Parser, run, (|=), (|.))

parse input = Parser.run conjunction input

conjunction : Parser Term
conjunction = 
   manySeparatedBy Parser.spaces word |> Parser.map Conjunction

{-|
    > run word "foo"
      Ok (Word "foo") : Result (List DeadEnd) API.Term

    > run word "-bar"
      Ok (NotWord "bar")
-}

word: Parser Term 
word = Parser.oneOf [negativeWord, positiveWord]

{-| 
    > run positiveWord "foo bar"
      Ok (Word "foo") : Result (List DeadEnd) API.Term
-}
positiveWord : Parser Term
positiveWord = text (\c -> c /= ' ') (\c -> c /= ' ') |> Parser.map .content |> Parser.map Word



{-| 
   > run negativeWord "-foo"
     Ok (NotWord "foo") : Result (List DeadEnd) API.Term
-}
negativeWord : Parser Term
negativeWord =
  (Parser.succeed (\r -> r.content)
   |. Parser.symbol "-"
   |= text (\c -> c /= ' ') (\c -> c /= ' ')) 
   |> Parser.map NotWord



