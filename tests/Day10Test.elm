module Day10Test exposing (..)

import Day10 exposing (Direction(..), Tile(..), calculatePart1, getAt, getStart, parser)
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
                    (getStart map)
        ]
