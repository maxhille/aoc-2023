module Day5Test exposing (..)

import Day5 exposing (calculatePart1, calculatePart2, intersect, mapRanges, parser)
import Expect
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Day 5 tests"
        [ test "Calculate Part 1" <|
            \_ ->
                let
                    input =
                        """
                    seeds: 79 14 55 13

                    seed-to-soil map:
                    50 98 2
                    52 50 48

                    soil-to-fertilizer map:
                    0 15 37
                    37 52 2
                    39 0 15

                    fertilizer-to-water map:
                    49 53 8
                    0 11 42
                    42 0 7
                    57 7 4

                    water-to-light map:
                    88 18 7
                    18 25 70

                    light-to-temperature map:
                    45 77 23
                    81 45 19
                    68 64 13

                    temperature-to-humidity map:
                    0 69 1
                    1 0 69

                    humidity-to-location map:
                    60 56 37
                    56 93 4
                """
                in
                Expect.equal
                    (Ok 35)
                    (calculatePart1 input)
        , test "Calculate Part 2" <|
            \_ ->
                let
                    input =
                        """
                    seeds: 79 14 55 13

                    seed-to-soil map:
                    50 98 2
                    52 50 48

                    soil-to-fertilizer map:
                    0 15 37
                    37 52 2
                    39 0 15

                    fertilizer-to-water map:
                    49 53 8
                    0 11 42
                    42 0 7
                    57 7 4

                    water-to-light map:
                    88 18 7
                    18 25 70

                    light-to-temperature map:
                    45 77 23
                    81 45 19
                    68 64 13

                    temperature-to-humidity map:
                    0 69 1
                    1 0 69

                    humidity-to-location map:
                    60 56 37
                    56 93 4
                """
                in
                Expect.equal
                    (Ok 46)
                    (calculatePart2 input)
        , test "intersect with problematic result" <|
            \_ ->
                let
                    map =
                        { sourceRangeStart = 10
                        , destinationRangeStart = 0
                        , rangeLength = 3
                        }

                    range =
                        { start = 10
                        , length = 1
                        }
                in
                Expect.equal
                    ( [ { start = 0, length = 1 } ]
                    , []
                    )
                    (intersect map range)
        , test "Parse Almanac" <|
            \_ ->
                let
                    input =
                        """
                        seeds: 79 14 55 13

                        seed-to-soil map:
                        50 98 2

                        soil-to-fertilizer map:
                        0 15 37
                        37 52 2
                        """
                in
                Expect.equal
                    (Ok
                        { seeds =
                            [ { start = 79, length = 14 }
                            , { start = 55, length = 13 }
                            ]
                        , maps =
                            [ [ { destinationRangeStart = 50, sourceRangeStart = 98, rangeLength = 2 }
                              ]
                            , [ { destinationRangeStart = 0, sourceRangeStart = 15, rangeLength = 37 }
                              , { destinationRangeStart = 37, sourceRangeStart = 52, rangeLength = 2 }
                              ]
                            ]
                        }
                    )
                    (Parser.run parser input)
        ]
