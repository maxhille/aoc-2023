module Day12Test exposing (..)

import Day12 exposing (..)
import Expect
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Day 12 tests"
        [ test "Part 1 Example" <|
            \_ ->
                let
                    input =
                        """
                        ???.### 1,1,3
                        .??..??...?##. 1,1,3
                        ?#?#?#?#?#?#?#? 1,3,1,6
                        ????.#...#... 4,1,1
                        ????.######..#####. 1,6,5
                        ?###???????? 3,2,1
                        """
                in
                Expect.equal
                    (Ok 21)
                    (calculatePart1 input)
        , test "Parse Records" <|
            \_ ->
                let
                    input =
                        """
                        ???.### 1,1,3
                        """
                in
                Expect.equal
                    (Ok
                        [ { springs = [ Unknown, Unknown, Unknown, Working, Damaged, Damaged, Damaged ]
                          , groups = [ 1, 1, 3 ]
                          }
                        ]
                    )
                    (Parser.run parser input)
        ]
