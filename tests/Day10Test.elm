module Day10Test exposing (..)

import Day10 exposing (..)
import Expect
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Day 10 tests"
        [ test "Part 1 - Example 1" <|
            \_ ->
                let
                    input =
                        """
                        .....
                        .S-7.
                        .|.|.
                        .L-J.
                        .....
                        """
                in
                Expect.equal
                    (Ok 4)
                    (calculatePart1 input)
        , test "Part 1 - Example 2" <|
            \_ ->
                let
                    input =
                        """
                          ..F7.
                          .FJ|.
                          SJ.L7
                          |F--J
                          LJ...
                          """
                in
                Expect.equal
                    (Ok 8)
                    (calculatePart1 input)
        , test "Part 2 - Example 1" <|
            \_ ->
                let
                    input =
                        """
                           ...........
                           .S-------7.
                           .|F-----7|.
                           .||.....||.
                           .||.....||.
                           .|L-7.F-J|.
                           .|..|.|..|.
                           .L--J.L--J.
                           ...........
                           """
                in
                Expect.equal
                    (Ok 4)
                    (calculatePart2 input)
        , test "Part 2 - Example 2" <|
            \_ ->
                let
                    input =
                        """
                        FF7FSF7F7F7F7F7F---7
                        L|LJ||||||||||||F--J
                        FL-7LJLJ||||||LJL-77
                        F--JF--7||LJLJ7F7FJ-
                        L---JF-JLJ.||-FJLJJ7
                        |F|F-JF---7F7-L7L|7|
                        |FFJF7L7F-JF7|JL---7
                        7-L-JL7||F7|L7F-7F7|
                        L.L7LFJ|||||FJL7||LJ
                        L7JLJL-JLJLJL--JLJ.L
                        """
                in
                Expect.equal
                    (Ok 10)
                    (calculatePart2 input)
        , test "pipeAtPos" <|
            \_ ->
                let
                    input =
                        """
                        ...........
                        .S-------7.
                        .|F-----7|.
                        .||.....||.
                        .||.....||.
                        .|L-7.F-J|.
                        .|..|.|..|.
                        .L--J.L--J.
                        ...........
                        """
                in
                Expect.equal
                    (Ok ( E, S ))
                    (input
                        |> Parser.run parser
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen
                            (\map ->
                                startPosition map
                                    |> Result.andThen (\start -> pipeAtPos start map)
                            )
                    )
        , test "Parse Map" <|
            \_ ->
                let
                    input =
                        """
                        SJLL7
                        |F--J
                        """
                in
                Expect.equal
                    (Ok
                        [ [ Start, Pipe ( N, W ), Pipe ( N, E ), Pipe ( N, E ), Pipe ( S, W ) ]
                        , [ Pipe ( N, S ), Pipe ( E, S ), Pipe ( E, W ), Pipe ( E, W ), Pipe ( N, W ) ]
                        ]
                    )
                    (Parser.run parser input)
        , test "getAt" <|
            \_ ->
                let
                    map =
                        [ [ Start, Pipe ( N, W ), Pipe ( N, E ), Pipe ( N, E ), Pipe ( S, W ) ]
                        , [ Pipe ( N, S ), Pipe ( E, S ), Pipe ( E, W ), Pipe ( E, W ), Pipe ( N, W ) ]
                        ]
                in
                Expect.equal
                    (Pipe ( E, W ))
                    (getAt ( 2, 1 ) map)
        , test "getStart" <|
            \_ ->
                let
                    map =
                        [ [ Ground, Pipe ( N, W ), Pipe ( N, E ), Pipe ( N, E ), Pipe ( S, W ) ]
                        , [ Pipe ( N, S ), Pipe ( E, S ), Start, Pipe ( E, W ), Pipe ( N, W ) ]
                        ]
                in
                Expect.equal
                    (Ok ( 2, 1 ))
                    (startPosition map)
        ]
