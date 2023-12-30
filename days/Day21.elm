module Day21 exposing (Field(..), calculatePart1, calculatePart2, parser, puzzle)

import Array exposing (Array)
import Parser exposing ((|.), (|=), Parser, Trailing(..), oneOf, succeed, symbol)
import Puzzle exposing (Puzzle)
import Set exposing (Set)


type alias Garden =
    Array (Array Field)


type alias Position =
    ( Int, Int )


type Field
    = Start
    | Plot
    | Rocks


calculatePart1 : Int -> Garden -> Result String Int
calculatePart1 steps garden =
    startPosition garden
        |> Result.map
            (\position ->
                wander position steps garden
                    |> Set.size
            )


calculatePart2 : Int -> Garden -> Result String Int
calculatePart2 steps garden =
    let
        height =
            131

        repeats =
            steps // height

        outerReachable =
            (wander ( 0, 0 ) 65 garden |> Set.size)
                + (wander ( height - 1, 0 ) 65 garden |> Set.size)
                + (wander ( height - 1, height - 1 ) 65 garden |> Set.size)
                + (wander ( 0, height - 1 ) 65 garden |> Set.size)

        centerReachable =
            wander ( 65, 65 ) (modBy height steps) garden
                |> Set.size

        repeatedCenterReachable =
            ceiling (toFloat ((repeats * 2 + 1) ^ 2) * 0.5) * centerReachable

        repeatedOuterReachable =
            floor (toFloat ((repeats * 2 + 1) ^ 2) * 0.5) * outerReachable
    in
    Ok (repeatedCenterReachable + repeatedOuterReachable)
        |> Result.andThen (\_ -> Err "my solution seems to miss something and does not produce the correct answer :-(. I moved on to the next puzzle")


wander : Position -> Int -> Garden -> Set Position
wander start steps garden =
    wanderHelp (Set.singleton start) steps garden


wanderHelp : Set Position -> Int -> Garden -> Set Position
wanderHelp positions steps garden =
    if steps == 0 then
        positions

    else
        let
            newPositions =
                Set.foldl (\position acc -> walk position garden |> Set.union acc) Set.empty positions
        in
        wanderHelp newPositions (steps - 1) garden


walk : Position -> Garden -> Set Position
walk ( x, y ) garden =
    [ ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
        |> List.foldl
            (\position acc ->
                case fieldAt position garden of
                    Just Plot ->
                        Set.insert position acc

                    Just Start ->
                        Set.insert position acc

                    _ ->
                        acc
            )
            Set.empty


startPosition : Garden -> Result String Position
startPosition =
    positionedFold
        (\position field result ->
            case result of
                Ok _ ->
                    result

                Err _ ->
                    if field == Start then
                        Ok position

                    else
                        result
        )
        (Err "no start position found")


positionedFold : (Position -> Field -> a -> a) -> a -> Garden -> a
positionedFold fn a garden =
    Array.foldl
        (\( y, row ) acc ->
            Array.foldl
                (\( x, field ) acc_ ->
                    fn ( x, y ) field acc_
                )
                acc
                (Array.indexedMap Tuple.pair
                    row
                )
        )
        a
        (Array.indexedMap Tuple.pair garden)


fieldAt : Position -> Garden -> Maybe Field
fieldAt ( x, y ) =
    Array.get y >> Maybe.andThen (Array.get x)


parser : Parser Garden
parser =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = ""
        , spaces = Parser.chompWhile (always False)
        , item = rowParser
        , trailing = Optional
        }
        |> Parser.map Array.fromList


rowParser : Parser (Array Field)
rowParser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = Parser.chompWhile (always False)
        , item =
            oneOf
                [ succeed Start
                    |. symbol "S"
                , succeed Plot
                    |. symbol "."
                , succeed Rocks
                    |. symbol "#"
                ]
        , trailing = Optional
        }
        |> Parser.map Array.fromList


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen (calculatePart1 64)
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen (calculatePart2 26501365)
    }
