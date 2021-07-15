module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Parse exposing(parse)
import APITypes exposing(Datum, Term(..))
import Search exposing(search)
import Filter exposing(sort, SortParam(..), Direction(..))
import Time
import Search exposing (SearchConfig(..), posixGTE, posixLTE, posixGTEForData, posixLTEForData)




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
            ,  skip <|  test "date search (0)" <|
             \_ ->
                search NotCaseSensitive "@before:9/1/2001" [d1, d2, d3, d4, d5]
                    |> Expect.equal [d1, d2, d3]

          ,  skip <| test "date search (1)" <|
             \_ ->
                search NotCaseSensitive "@after:9/1/2001" [d1, d2, d3, d4, d5]
                    |> Expect.equal [d3, d4, d5]

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
            
            , test "posixGTE (1)" <| 
               \_ -> posixGTE august july
                |> Expect.equal True

             , test "posixGTE (2)" <| 
               \_ -> posixGTE july august
                |> Expect.equal False

           , test "posixLTE (1)" <| 
               \_ -> posixLTE august july
                |> Expect.equal False

            , test "posixLTE (2)" <| 
               \_ -> posixLTE july august
                |> Expect.equal True





           , test "posixLTEForData (1)" <|
             \_ -> posixLTEForData augustDatum june
             |> Expect.equal False


           , test "posixLTEForData (2)" <|
             \_ -> posixLTEForData juneDatum august
             |> Expect.equal True



           , test "posixGTEForData (1)" <|
             \_ -> posixGTEForData augustDatum june
             |> Expect.equal True


           , test "posixGTEForData (2)" <|
             \_ -> posixGTEForData juneDatum august
             |> Expect.equal False

        ]


d1 = { content = "alpha beta ", dateTime =  june } 


d2 = { content = "alpha foo bar xx yy", dateTime =  july }  

d2b = { content = "gamma xx yy", dateTime = august } 

d3 = { content = "beta foo beta Alpha xx yy", dateTime = september } 

d4= { content = "bar yada", dateTime =  october } 

d5= { content = "doo yada", dateTime = november } 


juneDatum = { content = "JUNE", dateTime =  june } 
julyDatum = { content = "JULY", dateTime =  july } 

augustDatum = { content = "AUGUST", dateTime =  august } 

june = Time.millisToPosix 991439999000
july = Time.millisToPosix 994031999000

august = Time.millisToPosix 996710399000

september = Time.millisToPosix 999388799000


october = Time.millisToPosix 1001980799000

november = Time.millisToPosix 1004659199000

data = [d1, d2, d3, d4, d5]

data1 = [d3]