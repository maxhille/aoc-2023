module Day5 exposing (calculatePart1, main, parser, seedsParser)

import Dict exposing (Dict)
import Html exposing (text)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Trailing(..)
        , chompWhile
        , int
        , problem
        , sequence
        , spaces
        , succeed
        , token
        )
import Ui exposing (Ui, ui)


type alias Range =
    { destinationRangeStart : Int
    , sourceRangeStart : Int
    , length : Int
    }


type alias Map =
    { destinationType : String
    , ranges : List Range
    }


type alias Almanac =
    { seeds : List Int
    , maps : Dict String Map
    }


calculatePart1 : String -> Result (List Parser.DeadEnd) Int
calculatePart1 input =
    Parser.run
        (parser
            |> Parser.andThen
                (\almanac ->
                    seedLocations almanac
                        |> List.filterMap identity
                        |> List.minimum
                        |> (\maybe ->
                                case maybe of
                                    Just location ->
                                        succeed location

                                    Nothing ->
                                        problem "No location found"
                           )
                )
        )
        input


seedLocations : Almanac -> List (Maybe Int)
seedLocations almanac =
    almanac.seeds
        |> List.map (\seed -> Just ( seed, "seed" ))
        |> List.map (track almanac.maps)


track : Dict String Map -> Maybe ( Int, String ) -> Maybe Int
track maps =
    Maybe.andThen
        (\( sourceNumber, sourceType ) ->
            if sourceType == "location" then
                Just sourceNumber

            else
                maps
                    |> Dict.get sourceType
                    |> Maybe.map (\map -> ( mapSourceNumber sourceNumber map.ranges, map.destinationType ))
                    |> track maps
        )


mapSourceNumber : Int -> List Range -> Int
mapSourceNumber number ranges =
    List.foldr
        (\range hit ->
            case hit of
                Just int ->
                    Just int

                Nothing ->
                    if number >= range.sourceRangeStart && number < range.sourceRangeStart + range.length then
                        Just <| number + range.destinationRangeStart - range.sourceRangeStart

                    else
                        Nothing
        )
        Nothing
        ranges
        |> Maybe.withDefault number


parser : Parser Almanac
parser =
    succeed (\seeds maps -> { seeds = seeds, maps = Dict.fromList maps })
        |= seedsParser
        |= sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = spaces
            , item = mapParser
            , trailing = Optional
            }


seedsParser : Parser (List Int)
seedsParser =
    succeed identity
        |. spaces
        |. token "seeds: "
        |= sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = spaces
            , item = int
            , trailing = Optional
            }


mapParser : Parser ( String, Map )
mapParser =
    succeed (\src dst ranges -> ( src, { destinationType = dst, ranges = ranges } ))
        |. spaces
        |= (Parser.getChompedString <| succeed () |. chompWhile (\c -> c /= '-'))
        |. token "-to-"
        |= (Parser.getChompedString <| succeed () |. chompWhile (\c -> c /= ' '))
        |. token " map:"
        |= sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = spaces
            , item = rangeParser
            , trailing = Optional
            }


rangeParser : Parser Range
rangeParser =
    succeed Range
        |= int
        |. spaces
        |= int
        |. spaces
        |= int


main : Ui
main =
    ui
        [ { title = "Part 1"
          , view =
                calculatePart1
                    >> (\result ->
                            case result of
                                Ok int ->
                                    text <| String.fromInt int

                                Err error ->
                                    text <| Parser.deadEndsToString error
                       )
          }
        ]
