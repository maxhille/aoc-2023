module Day6Test exposing (..)

import Day6 exposing (calculatePart1, calculatePart2, parser, winterval)
import Expect
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Day 6 tests"
        [ test "Calculate Part 1" <|
            \_ ->
                let
                    input =
                        """
                        Time:      7  15   30
                        Distance:  9  40  200
                        """
                in
                Expect.equal
                    (Ok 288)
                    (calculatePart1 input)
        , test "Calculate Part 2" <|
            \_ ->
                let
                    input =
                        """
                        Time:      7  15   30
                        Distance:  9  40  200
                        """
                in
                Expect.equal
                    (Ok 71503)
                    (calculatePart2 input)
        , test "Test winterval" <|
            \_ ->
                let
                    race =
                        { time = 15, distance = 40 }
                in
                Expect.equal
                    ( 4, 11 )
                    (winterval race)
        , test "Test winterval race 3" <|
            \_ ->
                let
                    race =
                        { time = 30, distance = 200 }
                in
                Expect.equal
                    ( 11, 19 )
                    (winterval race)
        , test "Parse Sheet" <|
            \_ ->
                let
                    input =
                        """
                        Time:      7  15   30
                        Distance:  9  40  200
                        """
                in
                Expect.equal
                    (Ok
                        [ { time = 7, distance = 9 }
                        , { time = 15, distance = 40 }
                        , { time = 30, distance = 200 }
                        ]
                    )
                    (Parser.run parser input)
        ]
