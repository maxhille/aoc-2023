module Day13Test exposing (..)

import Day13 exposing (..)
import Expect
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 13 tests"
        [ test "Part 1 - Example 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                                 #.##..##.
                                 ..#.##.#.
                                 ##......#
                                 ##......#
                                 ..#.##.#.
                                 ..##..##.
                                 #.#.##.#.
                                 """
                in
                Expect.equal
                    (Ok 5)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart1
                    )
        , test "Part 1 - Example 2" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                             #...##..#
                             #....#..#
                             ..##..###
                             #####.##.
                             #####.##.
                             ..##..###
                             #....#..#
                             """
                in
                Expect.equal
                    (Ok 400)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart1
                    )
        , test "perfectReflection" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            #...##..#
                            #....#..#
                            ..##..###
                            #####.##.
                            #####.##.
                            ..##..###
                            #....#..#
                            """
                in
                Expect.equal
                    (Just 4)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.toMaybe
                        |> Maybe.andThen List.head
                        |> Maybe.andThen horizontalReflection
                    )
        , test "parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            #.
                            .#

                            ..#
                            ###
                            ..#
                            """
                in
                Expect.equal
                    (Ok
                        [ [ [ Rocks, Ash ]
                          , [ Ash, Rocks ]
                          ]
                        , [ [ Ash, Ash, Rocks ]
                          , [ Rocks, Rocks, Rocks ]
                          , [ Ash, Ash, Rocks ]
                          ]
                        ]
                    )
                    (Parser.run parser input)
        ]
