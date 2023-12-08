module Day1 exposing (calculatePart1, calculatePart2, first, last, puzzle, valuesPart2)

import Puzzle exposing (Puzzle)


type alias Value =
    { int : Int
    , str : String
    }


type alias Error =
    List String


calculatePart1 : String -> Result Error Int
calculatePart1 =
    calculate valuesPart1


calculatePart2 : String -> Result Error Int
calculatePart2 =
    calculate valuesPart2


valuesPart1 : List Value
valuesPart1 =
    [ ( 1, "1" )
    , ( 2, "2" )
    , ( 3, "3" )
    , ( 4, "4" )
    , ( 5, "5" )
    , ( 6, "6" )
    , ( 7, "7" )
    , ( 8, "8" )
    , ( 9, "9" )
    ]
        |> List.map (\( int, str ) -> { int = int, str = str })


valuesPart2 : List Value
valuesPart2 =
    [ ( 1, [ "1", "one" ] )
    , ( 2, [ "2", "two" ] )
    , ( 3, [ "3", "three" ] )
    , ( 4, [ "4", "four" ] )
    , ( 5, [ "5", "five" ] )
    , ( 6, [ "6", "six" ] )
    , ( 7, [ "7", "seven" ] )
    , ( 8, [ "8", "eight" ] )
    , ( 9, [ "9", "nine" ] )
    ]
        |> List.map (\( int, strs ) -> List.map (\str -> { int = int, str = str }) strs)
        |> List.concat


parse : List Value -> String -> Maybe Int
parse values_ input =
    values_
        |> List.map (\value -> { index = String.indexes value.str input |> List.head, int = value.int })
        |> List.filterMap (\{ index, int } -> Maybe.map (\justIndex -> { index = justIndex, int = int }) index)
        |> List.sortBy .index
        |> List.map .int
        |> List.head


first : List Value -> String -> Maybe Int
first values =
    parse values


last : List Value -> String -> Maybe Int
last values =
    let
        reversed =
            values |> List.map (\{ int, str } -> { int = int, str = String.reverse str })
    in
    String.reverse >> parse reversed


toLines : String -> List String
toLines =
    String.trim >> String.lines >> List.map String.trim


calculate : List Value -> String -> Result Error Int
calculate values input =
    let
        add : String -> Line
        add line =
            case ( first values line, last values line ) of
                ( Just x, Just y ) ->
                    GoodLine <| x * 10 + y

                ( _, _ ) ->
                    BadLine line
    in
    input
        |> toLines
        |> List.map add
        |> List.foldl
            (\line acc ->
                case line of
                    BadLine badLine ->
                        { acc | errors = badLine :: acc.errors }

                    GoodLine value ->
                        { acc | value = acc.value + value }
            )
            { value = 0
            , errors = []
            }
        |> (\{ value, errors } ->
                if errors == [] then
                    Ok value

                else
                    Err errors
           )


type Line
    = BadLine String
    | GoodLine Int


puzzle : Puzzle
puzzle =
    { validate = \_ -> Ok ""
    , calculatePart1 = calculatePart1 >> Result.mapError (String.join "; ")
    , calculatePart2 = calculatePart2 >> Result.mapError (String.join "; ")
    }
