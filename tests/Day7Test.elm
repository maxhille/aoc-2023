module Day7Test exposing (..)

import Day7 exposing (Hand(..), Strength(..), calculatePart1, entryParser, strength)
import Expect
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Day 7 tests"
        [ test "Calculate Part 1" <|
            \_ ->
                let
                    input =
                        """
                        32T3K 765
                        T55J5 684
                        KK677 28
                        KTJJT 220
                        QQQJA 483
                        """
                in
                Expect.equal
                    (Ok 6440)
                    (calculatePart1 input)
        , test "strength" <|
            \_ ->
                let
                    hand =
                        Hand '3' '3' '3' '3' '3'
                in
                Expect.equal
                    FiveOfAKind
                    (strength hand)
        , test "Parse Entry" <|
            \_ ->
                let
                    input =
                        "32T3K 765"
                in
                Expect.equal
                    (Ok
                        { hand = Hand '3' '2' 'T' '3' 'K', bid = 765 }
                    )
                    (Parser.run entryParser input)
        ]
