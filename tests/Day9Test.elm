module Day9Test exposing (..)

import Day9 exposing (calculatePart1, derive, difference, historyParser, parser)
import Expect
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Day 9 tests"
        [ test "Part 1 - Example 1" <|
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
        , test "derive" <|
            \_ ->
                let
                    input =
                        [ 0, 3, 6, 9, 12, 15 ]
                in
                Expect.equal
                    18
                    (derive 0 input)
        , test "derive 2" <|
            \_ ->
                let
                    input =
                        [ 10, 13, 16, 21, 30, 45 ]
                in
                Expect.equal
                    68
                    (derive 0 input)
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
