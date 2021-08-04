module Example exposing (..)

--- (SearchConfig(..), mySearch)

import APITypes exposing (Term(..), SearchTarget)
import Expect exposing (Expectation)
import Parse exposing (parse)
import Search exposing (..)
import Sort exposing (Direction(..), SortParam(..), sort)
import Test exposing (..)
import Time exposing (Posix)

transform :{ content : String, dateTime : Time.Posix } -> SearchTarget
transform { content, dateTime } = {targetContent = content , targetDate= dateTime }

mySearch : SearchConfig -> String -> List { content : String, dateTime : Posix } -> List { content : String, dateTime : Posix }
mySearch = search transform

mySearchWithTerm : SearchConfig -> Term -> List { content : String, dateTime : Posix } -> List { content : String, dateTime : Posix }
mySearchWithTerm = searchWithTerm transform

mySort : SortParam -> List { content : String, dateTime : Posix } -> List { content : String, dateTime : Posix }
mySort = sort transform


suite : Test
suite =
    describe "API"
        [ test "parser" <|
            \_ ->
                parse "foo -bar"
                    |> Expect.equal (Ok (Conjunction [ Word "foo", NotWord "bar" ]))
        , test "conjunctive mySearch (2)" <|
            \_ ->
                mySearch NotCaseSensitive  "foo -bar" data
                    |> Expect.equal [ d3 ]
        , test "conjunctive mySearch (1)" <|
            \_ ->
                mySearch NotCaseSensitive  "foo bar" data
                    |> Expect.equal [ d2 ]
        , test "conjunctive mySearch (0)" <|
            \_ ->
                mySearch NotCaseSensitive  "foo" data
                    |> Expect.equal [ d2, d3 ]
        , test "date mySearch (0)" <|
            \_ ->
                mySearch NotCaseSensitive  "@before:9/1/2021" [ d1, d2, d3, d4, d5 ]
                    |> Expect.equal [ d1, d2, d3 ]
        , test "date mySearch (1)" <|
            \_ ->
                mySearch NotCaseSensitive  "@after:9/1/2021" [ d1, d2, d3, d4, d5 ]
                    |> Expect.equal [ d3, d4, d5 ]
        , test "case sensitive conjunctive mySearch (0)" <|
            \_ ->
                mySearch CaseSensitive  "Foo" data
                    |> Expect.equal []
        , test "mySort (1)" <|
            \_ ->
                mySort (Alpha Increasing)  [ d2, d2b, d3 ]
                    |> Expect.equal [ d2, d3, d2b ]
        , test "mySort (d)" <|
            \_ ->
                mySort (Alpha Decreasing)  [ d2, d2b, d3 ]
                    |> Expect.equal (List.reverse [ d2, d3, d2b ])
        , test "mySort date, ascending" <|
            \_ ->
                mySort (DateTime Increasing)  [ d3, d4, d5, d1, d2 ]
                    |> Expect.equal [ d1, d2, d3, d4, d5 ]

        , test "mySort date, descending" <|
            \_ ->
                mySort (DateTime Decreasing)   [ d3, d4, d5, d1, d2 ]
                    |> Expect.equal (List.reverse [ d1, d2, d3, d4, d5 ])

        , test "mySearchWithTerm (1)" <|
            \_ ->
                mySearchWithTerm CaseSensitive  beforeJuly_ [ juneDatum ]
                    |> Expect.equal [ { content = "JUNE", dateTime = Time.millisToPosix 1622591999000 } ]
        , test "mySearchWithTerm (2)" <|
            \_ ->
                mySearchWithTerm CaseSensitive  afterJuly_ [ juneDatum ]
                    |> Expect.equal []
        , test "mySearchWithTerm (3)" <|
            \_ ->
                mySearchWithTerm CaseSensitive  (Conjunction [ Word "foo", Word "bar" ]) data
                    |> Expect.equal [ d2 ]
        , test "mySearch (1)" <|
            \_ ->
                mySearch CaseSensitive  "@before:7/1/2021" [ juneDatum ]
                    |> Expect.equal [ { content = "JUNE", dateTime = Time.millisToPosix 1622591999000 } ]
        , test "mySearch (2)" <|
            \_ ->
                mySearch CaseSensitive  "@after:7/1/2021" [ juneDatum ]
                    |> Expect.equal []
        , test "colors (1)" <|
            \_ ->
                mySearch CaseSensitive  "@before:9/1/2021" colors
                    |> List.length
                    |> Expect.equal 4
        , test "colors (2)" <|
            \_ ->
                mySearch CaseSensitive  "@after:9/1/2021" colors
                    |> List.length
                    |> Expect.equal 3
        , test "colors (3)" <|
            \_ ->
                mySearch CaseSensitive  "@range:7/1/2021:9/1/2021" colors
                    |> List.length
                    |> Expect.equal 3
        ]



d1 =
    { content = "alpha beta ", dateTime = june }


d2 =
    { content = "alpha foo bar xx yy", dateTime = july }


d2b =
    { content = "gamma xx yy", dateTime = august }


d3 =
    { content = "beta foo beta Alpha xx yy", dateTime = september }


d4 =
    { content = "bar yada", dateTime = october }


d5 =
    { content = "doo yada", dateTime = november }

e1 =
    { content = "AAA", dateTime = october }


e2 =
    { content = "BBB", dateTime = november }


beforeJuly_ =
    BeforeDateTime (Time.millisToPosix 1625183999000)


afterJuly_ =
    AfterDateTime (Time.millisToPosix 1625183999000)


juneDatum =
    { content = "JUNE", dateTime = june }


julyDatum =
    { content = "JULY", dateTime = july }


augustDatum =
    { content = "AUGUST", dateTime = august }


june =
    Time.millisToPosix 1622591999000


july =
    Time.millisToPosix 1625183999000


august =
    Time.millisToPosix 1627862399000


september =
    Time.millisToPosix 1630540799000


october = Time.millisToPosix 1633132799000


november = Time.millisToPosix 1635811199000


data =
    [ d1, d2, d3, d4, d5 ]


data1 =
    [ d3 ]



-- COLORS


colors =
    [ c1, c2, c3, c4, c5, c6 ]


c1 =
    { content = "alizarin yellow", dateTime = june }


c2 =
    { content = "brown umber", dateTime = july }


c3 =
    { content = "yellow ochre", dateTime = august }


c4 =
    { content = "pthalo blue", dateTime = september }


c5 =
    { content = "french yellow", dateTime = october }


c6 =
    { content = "alizarin crimson, cadmium purple", dateTime = november }
