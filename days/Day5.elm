module Day5 exposing (calculatePart1, calculatePart2, intersect, mapRanges, parser, puzzle)

import List.Extra
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
import Puzzle exposing (Puzzle)


type alias RangeMap =
    { destinationRangeStart : Int
    , sourceRangeStart : Int
    , rangeLength : Int
    }


type alias Range =
    { start : Int
    , length : Int
    }


type alias Almanac =
    { seeds : List Range
    , maps : List (List RangeMap)
    }


calculatePart1 : String -> Result (List Parser.DeadEnd) Int
calculatePart1 input =
    calculatePart input
        (\almanac ->
            { almanac
                | seeds =
                    almanac.seeds
                        |> List.map
                            (\{ start, length } ->
                                [ { start = start, length = 1 }
                                , { start = length, length = 1 }
                                ]
                            )
                        >> List.concat
            }
        )


calculatePart2 : String -> Result (List Parser.DeadEnd) Int
calculatePart2 input =
    calculatePart input identity


calculatePart : String -> (Almanac -> Almanac) -> Result (List Parser.DeadEnd) Int
calculatePart input transform =
    Parser.run
        (parser
            |> Parser.map transform
            |> Parser.andThen
                (\almanac ->
                    seedLocations almanac
                        |> List.map .start
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


seedLocations : Almanac -> List Range
seedLocations almanac =
    List.foldl mapRanges almanac.seeds almanac.maps


mapRanges : List RangeMap -> List Range -> List Range
mapRanges maps ranges =
    List.map
        (\range ->
            case maps of
                map :: mapsRemainder ->
                    let
                        ( mappedRanges, rangeRemainder ) =
                            intersect map range
                    in
                    mappedRanges ++ mapRanges mapsRemainder rangeRemainder

                [] ->
                    [ range ]
        )
        ranges
        |> List.concat
        |> List.Extra.unique


intersect : RangeMap -> Range -> ( List Range, List Range )
intersect map range =
    let
        mapStart =
            map.sourceRangeStart

        mapEnd =
            map.sourceRangeStart + map.rangeLength - 1

        rangeStart =
            range.start

        rangeEnd =
            range.start + range.length - 1

        shift =
            map.destinationRangeStart - map.sourceRangeStart

        prefix =
            let
                end =
                    min (mapStart - 1) rangeEnd

                length =
                    end - range.start + 1
            in
            if length > 0 then
                [ { range
                    | start = range.start
                    , length = length
                  }
                ]

            else
                []

        mapped =
            let
                start =
                    max rangeStart mapStart

                end =
                    min rangeEnd mapEnd

                length =
                    end - start + 1
            in
            if length > 0 then
                [ { range
                    | start = start + shift
                    , length = length
                  }
                ]

            else
                []

        postfix =
            let
                start =
                    max (mapEnd + 1) rangeStart

                length =
                    rangeEnd - start + 1
            in
            if length > 0 then
                [ { range
                    | start = start
                    , length = length
                  }
                ]

            else
                []
    in
    ( mapped, List.concat [ prefix, postfix ] )


parser : Parser Almanac
parser =
    succeed Almanac
        |= seedsParser
        |= sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = spaces
            , item = mapParser
            , trailing = Optional
            }


seedsParser : Parser (List Range)
seedsParser =
    succeed identity
        |. spaces
        |. token "seeds: "
        |= sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = spaces
            , item =
                Parser.succeed Range
                    |= int
                    |. spaces
                    |= int
            , trailing = Optional
            }


mapParser : Parser (List RangeMap)
mapParser =
    succeed identity
        |. spaces
        |. chompWhile (\c -> c /= '-')
        |. token "-to-"
        |. chompWhile (\c -> c /= ' ')
        |. token " map:"
        |= sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = spaces
            , item = rangeMapParser
            , trailing = Optional
            }


rangeMapParser : Parser RangeMap
rangeMapParser =
    succeed RangeMap
        |= int
        |. spaces
        |= int
        |. spaces
        |= int


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "Got Mappings") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = calculatePart1 >> Result.mapError Parser.deadEndsToString
    , calculatePart2 = calculatePart2 >> Result.mapError Parser.deadEndsToString
    }
