module Day20Test exposing (..)

import Day20 exposing (..)
import Dict
import Expect
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 20 tests"
        [ test "Part 1 - Example 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            broadcaster -> a, b, c
                            %a -> b
                            %b -> c
                            %c -> inv
                            &inv -> a
                            """
                in
                Expect.equal
                    (Ok 32000000)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart1
                    )
        , test "Part 1 - Example 2" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                           broadcaster -> a
                           %a -> inv, con
                           &inv -> b
                           %b -> con
                           &con -> output
                           """
                in
                Expect.equal
                    (Ok 11687500)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart1
                    )
        , test "toNetwork" <|
            \_ ->
                let
                    defs =
                        [ BroadcasterDef [ "a", "b", "c" ]
                        , FlipflopDef "a" [ "b" ]
                        , FlipflopDef "b" [ "c" ]
                        , FlipflopDef "c" [ "inv" ]
                        , ConjunctionDef "inv" [ "a" ]
                        ]
                in
                Expect.equal
                    (Dict.fromList
                        [ ( "broadcaster", Broadcaster { outputs = [ "a", "b", "c" ] } )
                        , ( "a", Flipflop { state = Off, outputs = [ "b" ] } )
                        , ( "b", Flipflop { state = Off, outputs = [ "c" ] } )
                        , ( "c", Flipflop { state = Off, outputs = [ "inv" ] } )
                        , ( "inv", Conjunction { outputs = [ "a" ], inputs = Dict.fromList [ ( "c", Low ) ] } )
                        ]
                    )
                    (toNetwork defs)
        , test "parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            broadcaster -> a, b, c
                            %a -> b
                            %b -> c
                            %c -> inv
                            &inv -> a
                            """
                in
                Expect.equal
                    (Ok
                        [ BroadcasterDef [ "a", "b", "c" ]
                        , FlipflopDef "a" [ "b" ]
                        , FlipflopDef "b" [ "c" ]
                        , FlipflopDef "c" [ "inv" ]
                        , ConjunctionDef "inv" [ "a" ]
                        ]
                    )
                    (Parser.run parser input)
        ]
