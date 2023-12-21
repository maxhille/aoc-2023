module Day15Test exposing (..)

import Day15 exposing (..)
import Expect
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 15 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
                            """
                in
                Expect.equal
                    (Ok 1320)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart1
                    )
        , test "parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            rn=1,cm-,qp=3,cm=2
                            """
                in
                Expect.equal
                    (Ok
                        [ "rn=1"
                        , "cm-"
                        , "qp=3"
                        , "cm=2"
                        ]
                    )
                    (Parser.run parser input)
        ]
