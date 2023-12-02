module Day1Test exposing (..)

import Day1 exposing (calculate, first, last)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Day 1 tests"
        [ test "Example calculation result - Part 1" <|
            \_ ->
                let
                    input =
                        """
                        1abc2
                        pqr3stu8vwx
                        a1b2c3d4e5f
                        treb7uchet
                        """
                in
                Expect.equal (Ok 142) (calculate input)
        , test "Example calculation result - Part 2" <|
            \_ ->
                let
                    input =
                        """
                        two1nine
                        eightwothree
                        abcone2threexyz
                        xtwone3four
                        4nineeightseven2
                        zoneight234
                        7pqrstsixteen
                        """
                in
                Expect.equal (Ok 281) (calculate input)
        , test "Edge case 1" <|
            \_ ->
                let
                    input =
                        """
                        eightwothree
                        """
                in
                Expect.equal (Just 8) (first input)
        , test "Edge case 2" <|
            \_ ->
                let
                    input =
                        """
                        oonethreee
                        """
                in
                Expect.equal (Just 3) (last input)
        ]
