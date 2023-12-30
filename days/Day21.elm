module Day21 exposing (Field(..), calculatePart1, calculatePart2, parser, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..), oneOf, succeed, symbol)
import Puzzle exposing (Puzzle)
import Set exposing (Set)


type alias Garden =
    List (List Field)


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


calculatePart2 : Garden -> Result String Int
calculatePart2 _ =
    Err "not implemented"


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
startPosition garden =
    List.foldl
        (\( y, row ) result ->
            case result of
                Nothing ->
                    List.foldl
                        (\( x, field ) result_ ->
                            if field == Start then
                                Just ( x, y )

                            else
                                result_
                        )
                        Nothing
                        (List.indexedMap Tuple.pair
                            row
                        )

                _ ->
                    result
        )
        Nothing
        (List.indexedMap Tuple.pair garden)
        |> Result.fromMaybe "no start position found"


fieldAt : Position -> Garden -> Maybe Field
fieldAt ( x, y ) =
    List.drop y >> List.head >> Maybe.andThen (List.drop x >> List.head)


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


rowParser : Parser (List Field)
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


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen (calculatePart1 64)
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
