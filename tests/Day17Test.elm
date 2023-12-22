module Day17Test exposing (..)

import Day17 exposing (..)
import Expect
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 17 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                let
                    input =
                        String.replace "X" "\\" <|
                            sanitize <|
                                """
                                2413432311323
                                3215453535623
            3255245654254
        3446585845452
        4546657867536
        1438598798454
        4457876987766
        3637877979653
        4654967986887
        4564679986453
        1224686865563
        2546548887735
        4322674655533
                                """
                in
                Expect.equal
                    (Ok 102)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart1
                    )
        , test "parser" <|
            \_ ->
                let
                    input =
                        String.replace "X" "\\" <|
                            sanitize <|
                                """
                                154
                                337
                                """
                in
                Expect.equal
                    (Ok
                        [ [ 1, 5, 4 ]
                        , [ 3, 3, 7 ]
                        ]
                    )
                    (Parser.run parser input)
        ]
