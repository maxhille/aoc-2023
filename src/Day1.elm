module Day1 exposing (calculate, first, last, main, values)

import Calculator exposing (Error(..), calculator)


type alias Value =
    { int : Int
    , str : String
    }


values : List Value
values =
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


first : String -> Maybe Int
first =
    parse values


last : String -> Maybe Int
last =
    let
        reversed =
            values |> List.map (\{ int, str } -> { int = int, str = String.reverse str })
    in
    String.reverse >> parse reversed


toLines : String -> List String
toLines =
    String.trim >> String.lines >> List.map String.trim


calculate : String -> Result Error Int
calculate input =
    let
        add : String -> Line
        add line =
            case ( first line, last line ) of
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
                    Err <| BadLines errors
           )


type Line
    = BadLine String
    | GoodLine Int


main : Program () Calculator.Model Calculator.Msg
main =
    calculator calculate
