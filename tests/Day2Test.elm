module Day2Test exposing (..)

import Day2 exposing (Color(..), parser, possibleSum, powerSum)
import Expect
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Day 2 tests"
        [ test "Calculate Part 1 Example" <|
            \_ ->
                let
                    input =
                        """
                        Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
                        Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
                        Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
                        Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
                        Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
                        """
                in
                Expect.equal
                    (Ok 8)
                    (possibleSum input)
        , test "Calculate Part 2 Example" <|
            \_ ->
                let
                    input =
                        """
                        Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
                        Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
                        Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
                        Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
                        Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
                        """
                in
                Expect.equal
                    (Ok 2286)
                    (powerSum input)
        , test "Parse Games" <|
            \_ ->
                let
                    input =
                        """
                        Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
                        Game 2: 3 blue, 4 red; 1 red, 3 green, 6 blue; 2 green
                        """
                in
                Expect.equal
                    (Ok
                        [ { id = 1
                          , reveals =
                                [ { red = 4, green = 0, blue = 3 }
                                , { red = 1, green = 2, blue = 6 }
                                , { red = 0, green = 2, blue = 0 }
                                ]
                          }
                        , { id = 2
                          , reveals =
                                [ { red = 4, green = 0, blue = 3 }
                                , { red = 1, green = 3, blue = 6 }
                                , { red = 0, green = 2, blue = 0 }
                                ]
                          }
                        ]
                    )
                    (Parser.run parser input)
        ]
