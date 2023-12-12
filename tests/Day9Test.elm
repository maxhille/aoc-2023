module Day9Test exposing (..)

import Day9 exposing (Mode(..), calculatePart1, calculatePart2, derive, difference, historyParser, parser)
import Expect
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Day 9 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                let
                    input =
                        """
                         0 3 6 9 12 15
                         1 3 6 10 15 21
                         10 13 16 21 30 45
                         """
                in
                Expect.equal
                    (Ok 114)
                    (calculatePart1 input)
        , test "Part 2 - Example" <|
            \_ ->
                let
                    input =
                        """
                         0 3 6 9 12 15
                         1 3 6 10 15 21
                         10 13 16 21 30 45
                         """
                in
                Expect.equal
                    (Ok 2)
                    (calculatePart2 input)
        , test "derive" <|
            \_ ->
                let
                    input =
                        [ 0, 3, 6, 9, 12, 15 ]
                in
                Expect.equal
                    [ 3, 15 ]
                    (derive Future [] input)
        , test "derive 2" <|
            \_ ->
                let
                    input =
                        [ 10, 13, 16, 21, 30, 45 ]
                in
                Expect.equal
                    [ 2, 6, 15, 45 ]
                    (derive Future [] input)
        , test "difference" <|
            \_ ->
                let
                    input =
                        [ 0, 3, 6, 9, 12, 15 ]
                in
                Expect.equal
                    [ 3, 3, 3, 3, 3 ]
                    (difference input)
        , test "difference2" <|
            \_ ->
                let
                    input =
                        [ 3, 3, 3, 3, 3 ]
                in
                Expect.equal
                    [ 0, 0, 0, 0 ]
                    (difference input)
        , test "Parse Histories" <|
            \_ ->
                let
                    input =
                        """
                        0 3 6 9 12 15
                        1 3 6 10 15 21
                        10 13 16 21 30 45
                        """
                in
                Expect.equal
                    (Ok
                        [ [ 0, 3, 6, 9, 12, 15 ]
                        , [ 1, 3, 6, 10, 15, 21 ]
                        , [ 10, 13, 16, 21, 30, 45 ]
                        ]
                    )
                    (Parser.run parser input)
        , test "Parse History" <|
            \_ ->
                let
                    input =
                        "0 3 6 9 12 -15"
                in
                Expect.equal
                    (Ok [ 0, 3, 6, 9, 12, -15 ])
                    (Parser.run historyParser input)
        ]
