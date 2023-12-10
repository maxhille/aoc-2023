module Day8Test exposing (..)

import Day8 exposing (Instruction(..), calculatePart1, instructionForStep, parser)
import Dict
import Expect
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Day 8 tests"
        [ test "Part 1 - Example 1" <|
            \_ ->
                let
                    input =
                        """
                        RL

                        AAA = (BBB, CCC)
                        BBB = (DDD, EEE)
                        CCC = (ZZZ, GGG)
                        DDD = (DDD, DDD)
                        EEE = (EEE, EEE)
                        GGG = (GGG, GGG)
                        ZZZ = (ZZZ, ZZZ)
                        """
                in
                Expect.equal
                    (Ok 2)
                    (calculatePart1 input)
        , test "Part 1 - Example 2" <|
            \_ ->
                let
                    input =
                        """
                        LLR

                        AAA = (BBB, BBB)
                        BBB = (AAA, ZZZ)
                        ZZZ = (ZZZ, ZZZ)
                        """
                in
                Expect.equal
                    (Ok 6)
                    (calculatePart1 input)
        , test "Next Instruction" <|
            \_ ->
                let
                    instructions =
                        [ Left, Left, Right ]
                in
                Expect.equal
                    [ Left, Left, Right, Left, Left ]
                    (List.map (\step -> instructionForStep step instructions) [ 0, 1, 2, 3, 4 ])
        , test "Parse Network" <|
            \_ ->
                let
                    input =
                        """
                        LLR

                        AAA = (BBB, BBB)
                        BBB = (AAA, ZZZ)
                        ZZZ = (ZZZ, ZZZ)
                        """
                in
                Expect.equal
                    (Ok
                        { instructions = [ Left, Left, Right ]
                        , forks =
                            Dict.fromList
                                [ ( "AAA", ( "BBB", "BBB" ) )
                                , ( "BBB", ( "AAA", "ZZZ" ) )
                                , ( "ZZZ", ( "ZZZ", "ZZZ" ) )
                                ]
                        }
                    )
                    (Parser.run parser input)
        ]
