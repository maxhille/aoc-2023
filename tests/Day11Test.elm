module Day11Test exposing (..)

import Day11 exposing (..)
import Expect
import Parser
import Set
import Test exposing (..)


suite : Test
suite =
    describe "Day 11 tests"
        [ test "Part 1 Example" <|
            \_ ->
                let
                    input =
                        """
                    ...#......
                    .......#..
                    #.........
                    ..........
                    ......#...
                    .#........
                    .........#
                    ..........
                    .......#..
                    #...#.....
                    """
                in
                Expect.equal
                    (Ok 374)
                    (calculatePart1 input)
        , test "Parse Image" <|
            \_ ->
                let
                    input =
                        """
                    ...#......
                    .......#..
                    #.........
                    ..........
                    ......#...
                    .#........
                    .........#
                    ..........
                    .......#..
                    #...#.....
                    """
                in
                Expect.equal
                    (Ok
                        (Set.fromList
                            [ ( 3, 0 ), ( 7, 1 ), ( 0, 2 ), ( 6, 4 ), ( 1, 5 ), ( 9, 6 ), ( 7, 8 ), ( 0, 9 ), ( 4, 9 ) ]
                        )
                    )
                    (Parser.run parser input)
        , test "expand" <|
            \_ ->
                let
                    image =
                        Set.fromList [ ( 1, 3 ), ( 2, 1 ), ( 2, 4 ) ]
                in
                Expect.equal
                    (Set.fromList [ ( 3, 2 ), ( 2, 5 ), ( 3, 6 ) ])
                    (expand 2 image)
        ]
