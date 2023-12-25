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
        , test "Part 2 - Example" <|
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
                    (Ok 952408144115)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart2
                    )
        , test "parser" <|
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
                                R 2 (#7807d2)
                                U 3 (#a77fa3)
                                L 2 (#015232)
                                U 2 (#7a21e3)
                                """
                in
                Expect.equal
                    (Ok
                        [ { direction = Right, distance = 6, colorInstruction = { distance = 461937, direction = Right } }
                        , { direction = Down, distance = 5, colorInstruction = { distance = 56407, direction = Down } }
                        , { direction = Left, distance = 2, colorInstruction = { distance = 356671, direction = Right } }
                        , { direction = Down, distance = 2, colorInstruction = { distance = 863240, direction = Down } }
                        , { direction = Right, distance = 2, colorInstruction = { distance = 367720, direction = Right } }
                        , { direction = Down, distance = 2, colorInstruction = { distance = 266681, direction = Down } }
                        , { direction = Left, distance = 5, colorInstruction = { distance = 577262, direction = Left } }
                        , { direction = Up, distance = 2, colorInstruction = { distance = 829975, direction = Up } }
                        , { direction = Right, distance = 2, colorInstruction = { distance = 491645, direction = Left } }
                        , { direction = Up, distance = 3, colorInstruction = { distance = 686074, direction = Up } }
                        , { direction = Left, distance = 2, colorInstruction = { distance = 5411, direction = Left } }
                        , { direction = Up, distance = 2, colorInstruction = { distance = 500254, direction = Up } }
                        ]
                    )
                    (Parser.run parser input)
        ]
