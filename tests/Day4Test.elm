module Day4Test exposing (..)

import Day4 exposing (calculatePart1, calculatePart2, parser)
import Expect
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Day 4 tests"
        [ test "Calculate Example Part 1" <|
            \_ ->
                let
                    input =
                        """
                        Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
                        Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
                        Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
                        Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
                        Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
                        Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
                        """
                in
                Expect.equal
                    (Ok 13)
                    (calculatePart1 input)
        , test "Calculate Example Part 2" <|
            \_ ->
                let
                    input =
                        """
                        Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
                        Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
                        Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
                        Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
                        Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
                        Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
                        """
                in
                Expect.equal
                    (Ok 30)
                    (calculatePart2 input)
        , test "Parse Cards" <|
            \_ ->
                let
                    input =
                        """
                        Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
                        """
                in
                Expect.equal
                    (Ok
                        [ { winning = [ 41, 48, 83, 86, 17 ]
                          , yours = [ 83, 86, 6, 31, 17, 9, 48, 53 ]
                          }
                        ]
                    )
                    (Parser.run parser input)
        ]
