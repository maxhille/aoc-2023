module Day21Test exposing (..)

import Day21 exposing (..)
import Expect
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 21 tests"
        [ test "Part 1 - Example 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            ...........
                            .....###.#.
                            .###.##..#.
                            ..#.#...#..
                            ....#.#....
                            .##..S####.
                            .##..#...#.
                            .......##..
                            .##.#.####.
                            .##..##.##.
                            ...........
                            """
                in
                Expect.equal
                    (Ok 16)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen (calculatePart1 6)
                    )
        , test "parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            .S
                            .#
                            """
                in
                Expect.equal
                    (Ok
                        [ [ Plot, Start ]
                        , [ Plot, Rocks ]
                        ]
                    )
                    (Parser.run parser input)
        ]
