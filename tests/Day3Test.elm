module Day3Test exposing (..)

import Day3 exposing (calculate)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Day 3 tests"
        [ test "Test Part 1 example" <|
            \_ ->
                let
                    input =
                        """
                            467..114..
                            ...*......
                            ..35..633.
                            ......#...
                            617*......
                            .....+.58.
                            ..592.....
                            ......755.
                            ...$.*....
                            .664.598..
                        """
                in
                Expect.equal
                    (Ok 4361)
                    (calculate input)
        ]
