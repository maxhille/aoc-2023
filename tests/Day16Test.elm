module Day16Test exposing (..)

import Day16 exposing (..)
import Expect
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 16 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                let
                    input =
                        String.replace "X" "\\" <|
                            sanitize <|
                                """
                                .|...X....
                                |.-.X.....
                                .....|-...
                                ........|.
                                ..........
                                .........X
                                ..../.XX..
                                .-.-/..|..
                                .|....-|.X
                                ..//.|....
                                """
                in
                Expect.equal
                    (Ok 46)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart1
                    )
        , test "Part 2 - Example" <|
            \_ ->
                let
                    input =
                        String.replace "X" "\\" <|
                            sanitize <|
                                """
                                .|...X....
                                |.-.X.....
                                .....|-...
                                ........|.
                                ..........
                                .........X
                                ..../.XX..
                                .-.-/..|..
                                .|....-|.X
                                ..//.|....
                                """
                in
                Expect.equal
                    (Ok 51)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart2
                    )
        , test "parser" <|
            \_ ->
                let
                    input =
                        String.replace "X" "\\" <|
                            sanitize <|
                                """
                                .|-
                                /X.
                                """
                in
                Expect.equal
                    (Ok
                        [ [ Empty, Vertical, Horizontal ]
                        , [ Slash, Backslash, Empty ]
                        ]
                    )
                    (Parser.run parser input)
        ]
