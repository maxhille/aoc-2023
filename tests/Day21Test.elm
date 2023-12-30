module Day21Test exposing (..)

import Array
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
                    ([ [ Plot, Start ] |> Array.fromList
                     , [ Plot, Rocks ] |> Array.fromList
                     ]
                        |> Array.fromList
                        |> Ok
                    )
                    (Parser.run parser input)
        ]
