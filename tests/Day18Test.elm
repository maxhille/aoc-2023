module Day18Test exposing (..)

import Day18 exposing (..)
import Expect
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 18 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                                R 6 (#70c710)
                                D 5 (#0dc571)
                                L 2 (#5713f0)
                                D 2 (#d2c081)
                                R 2 (#59c680)
                                D 2 (#411b91)
                                L 5 (#8ceee2)
                                U 2 (#caa173)
                                L 1 (#1b58a2)
                                U 2 (#caa171)
                                R 2 (#7807d2)
                                U 3 (#a77fa3)
                                L 2 (#015232)
                                U 2 (#7a21e3)
                                """
                in
                Expect.equal
                    (Ok 62)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart1
                    )
        , test "parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                                R 6 (#70c710)
                                D 5 (#0dc571)
                                """
                in
                Expect.equal
                    (Ok
                        [ { direction = Right, distance = 6, color = Color 0x70 0xC7 0x10 }
                        , { direction = Down, distance = 5, color = Color 0x0D 0xC5 0x71 }
                        ]
                    )
                    (Parser.run parser input)
        ]
