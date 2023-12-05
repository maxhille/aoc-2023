module Day5Test exposing (..)

import Day5 exposing (calculatePart1, parser, seedsParser)
import Dict
import Expect
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Day 5 tests"
        [ test "Part 1 Example" <|
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
        , test "Parse Seeds" <|
            \_ ->
                let
                    input =
                        """
                        seeds: 79 14 55 13
                        """
                in
                Expect.equal
                    (Ok
                        [ 79
                        , 14
                        , 55
                        , 13
                        ]
                    )
                    (Parser.run seedsParser input)
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
                        { seeds = [ 79, 14, 55, 13 ]
                        , maps =
                            Dict.fromList
                                [ ( "seed"
                                  , { destinationType = "soil"
                                    , ranges =
                                        [ { destinationRangeStart = 50, sourceRangeStart = 98, length = 2 }
                                        ]
                                    }
                                  )
                                , ( "soil"
                                  , { destinationType = "fertilizer"
                                    , ranges =
                                        [ { destinationRangeStart = 0, sourceRangeStart = 15, length = 37 }
                                        , { destinationRangeStart = 37, sourceRangeStart = 52, length = 2 }
                                        ]
                                    }
                                  )
                                ]
                        }
                    )
                    (Parser.run parser input)
        ]
