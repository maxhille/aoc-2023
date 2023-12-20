module Day14Test exposing (..)

import Day14 exposing (..)
import Expect
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 14 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            O....#....
                            O.OO#....#
                            .....##...
                            OO.#O....O
                            .O.....O#.
                            O.#..O.#.#
                            ..O..#O..O
                            .......O..
                            #....###..
                            #OO..#....
                            """
                in
                Expect.equal
                    (Ok 136)
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
                            O....#....
                            O.OO#....#
                            .....##...
                            OO.#O....O
                            .O.....O#.
                            O.#..O.#.#
                            ..O..#O..O
                            .......O..
                            #....###..
                            #OO..#....
                            """
                in
                Expect.equal
                    (Ok 64)
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
                            #O
                            .#
                            """
                in
                Expect.equal
                    (Ok
                        [ [ Cube, Rounded ]
                        , [ Empty, Cube ]
                        ]
                    )
                    (Parser.run parser input)
        ]
