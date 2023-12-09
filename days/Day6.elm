module Day6 exposing (calculatePart1, parser, puzzle, winterval)

import Parser exposing ((|.), (|=), Parser, Trailing(..), int, sequence, spaces, token)
import Puzzle exposing (Puzzle)


type alias Sheet =
    List Race


type alias Race =
    { time : Int
    , distance : Int
    }


calculatePart1 : String -> Result (List Parser.DeadEnd) Int
calculatePart1 input =
    Parser.run parser input
        |> Result.map
            (\sheet ->
                sheet
                    |> List.map winterval
                    |> List.map (\( t1, t2 ) -> t2 - t1 + 1)
                    |> List.product
            )


winterval : Race -> ( Int, Int )
winterval race =
    ( b1 race, b2 race )


b1 : Race -> Int
b1 { time, distance } =
    let
        t =
            toFloat time

        d =
            toFloat distance
    in
    (t / 2 - sqrt ((t / 2) ^ 2 - d))
        |> nextInt


b2 : Race -> Int
b2 { time, distance } =
    let
        t =
            toFloat time

        d =
            toFloat distance
    in
    (t / 2 + sqrt ((t / 2) ^ 2 - d))
        |> prevInt


nextInt : Float -> Int
nextInt f =
    if toFloat (round f) == f then
        ceiling f + 1

    else
        ceiling f


prevInt : Float -> Int
prevInt f =
    if toFloat (round f) == f then
        floor f - 1

    else
        floor f


parser : Parser Sheet
parser =
    Parser.succeed (List.map2 Race)
        |. spaces
        |= timesParser
        |= distancesParser


timesParser : Parser (List Int)
timesParser =
    Parser.succeed identity
        |. token "Time:"
        |= intsParser


distancesParser : Parser (List Int)
distancesParser =
    Parser.succeed identity
        |. token "Distance:"
        |= intsParser


intsParser : Parser (List Int)
intsParser =
    sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item = int
        , trailing = Optional
        }


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "Got Sheet") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = calculatePart1 >> Result.mapError Parser.deadEndsToString
    , calculatePart2 = (\_ -> Ok 3) >> Result.mapError Parser.deadEndsToString
    }
