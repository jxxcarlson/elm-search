module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Parse exposing(parse)
import APITypes exposing(Datum, Term(..))
import Search exposing(search)
import Filter exposing(sort, SortParam(..), Direction(..))
import Time
import Search exposing (SearchConfig(..))




suite : Test
suite =
    describe "API"
        [ test "parser" <|
            \_ ->
                parse "foo -bar"
                    |> Expect.equal (Ok (Conjunction [Word "foo",NotWord "bar"]))

           

            , test "conjunctive search (2)" <|
             \_ ->
                search NotCaseSensitive "foo -bar" data
                    |> Expect.equal [d3]

            , test "conjunctive search (1)" <|
             \_ ->
                search NotCaseSensitive "foo bar" data
                    |> Expect.equal [d2]


            ,  test "conjunctive search (0)" <|
             \_ ->
                search NotCaseSensitive "foo" data
                    |> Expect.equal [d2,d3]

            , test "case senstive conjunctive search (0)" <|
             \_ ->
                search CaseSensitive "Foo" data
                    |> Expect.equal []  
            , test "sort (1)"   <|
             \_ ->
                sort (Alpha Increasing) [d2, d2b, d3]
                    |> Expect.equal [d2, d3, d2b]  
            , test "sort (d)"   <|
             \_ ->
                sort (Alpha Decreasing) [d2, d2b, d3]
                    |> Expect.equal (List.reverse [d2, d3, d2b] )

            , test "sort date, ascending" <| 
            \_ -> sort (DateTime Increasing)  [d3, d4, d5, d1, d2]
              |> Expect.equal [d1, d2, d3, d4, d5]

            
            , test "sort date, descending" <| 
            \_ -> sort (DateTime Decreasing)  [d3, d4, d5, d1, d2]
              |> Expect.equal (List.reverse [d1, d2, d3, d4, d5])

        ]


d1 = { content = "alpha beta ", dateTime =  Time.millisToPosix 991439999000 } -- 6/1/2001


d2 = { content = "alpha foo bar xx yy", dateTime =  Time.millisToPosix 994031999000 }

d2b = { content = "gamma xx yy", dateTime = Time.millisToPosix 996710399000 }

d3 = { content = "beta foo beta Alpha xx yy", dateTime = Time.millisToPosix 999388799000 }

d4= { content = "bar yada", dateTime =  Time.millisToPosix 1001980799000 }

d5= { content = "doo yada", dateTime = Time.millisToPosix 1004659199000 }


data = [d1, d2, d3, d4, d5]

data1 = [d3]