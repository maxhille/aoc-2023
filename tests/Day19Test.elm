module Day19Test exposing (..)

import Day19 exposing (..)
import Expect
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 19 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                                 px{a<2006:qkq,m>2090:A,rfg}
                                 pv{a>1716:R,A}
                                 lnx{m>1548:A,A}
                                 rfg{s<537:gd,x>2440:R,A}
                                 qs{s>3448:A,lnx}
                                 qkq{x<1416:A,crn}
                                 crn{x>2662:A,R}
                                 in{s<1351:px,qqz}
                                 qqz{s>2770:qs,m<1801:hdj,R}
                                 gd{a>3333:R,R}
                                 hdj{m>838:A,pv}

                                 {x=787,m=2655,a=1222,s=2876}
                                 {x=1679,m=44,a=2067,s=496}
                                 {x=2036,m=264,a=79,s=2244}
                                 {x=2461,m=1339,a=466,s=291}
                                 {x=2127,m=1623,a=2188,s=1013}
                                 """
                in
                Expect.equal
                    (Ok 19114)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart1
                    )
        , test "Part 2 - Example" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                                 px{a<2006:qkq,m>2090:A,rfg}
                                 pv{a>1716:R,A}
                                 lnx{m>1548:A,A}
                                 rfg{s<537:gd,x>2440:R,A}
                                 qs{s>3448:A,lnx}
                                 qkq{x<1416:A,crn}
                                 crn{x>2662:A,R}
                                 in{s<1351:px,qqz}
                                 qqz{s>2770:qs,m<1801:hdj,R}
                                 gd{a>3333:R,R}
                                 hdj{m>838:A,pv}

                                 {x=787,m=2655,a=1222,s=2876}
                                 {x=1679,m=44,a=2067,s=496}
                                 {x=2036,m=264,a=79,s=2244}
                                 {x=2461,m=1339,a=466,s=291}
                                 {x=2127,m=1623,a=2188,s=1013}
                                 """
                in
                Expect.equal
                    (Ok 167409079868000)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart2
                    )
        , test "split 1" <|
            \_ ->
                Expect.equal
                    (OnlyTo { from = 100, to = 200 })
                    (split GT 99 { from = 100, to = 200 })
        , test "split 2" <|
            \_ ->
                Expect.equal
                    (OnlyNext { from = 100, to = 200 })
                    (split GT 200 { from = 100, to = 200 })
        , test "split 3" <|
            \_ ->
                Expect.equal
                    (Both { from = 151, to = 200 } { from = 100, to = 150 })
                    (split GT 150 { from = 100, to = 200 })
        , test "split 4" <|
            \_ ->
                Expect.equal
                    (Both { from = 70, to = 99 } { from = 100, to = 200 })
                    (split LT 100 { from = 70, to = 200 })
        , test "parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            px{a<2006:qkq,m>2090:A,rfg}

                            {x=787,m=2655,a=1222,s=2876}
                            """
                in
                Expect.equal
                    (Ok
                        { rules =
                            [ { id = "px"
                              , conditions =
                                    [ A LT 2006 (Next "qkq")
                                    , M GT 2090 Accept
                                    , Default (Next "rfg")
                                    ]
                              }
                            ]
                        , parts = [ { x = 787, m = 2655, a = 1222, s = 2876 } ]
                        }
                    )
                    (Parser.run parser input)
        ]
