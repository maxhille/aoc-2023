module Day14 exposing (Field(..), calculatePart1, calculatePart2, parser, puzzle)

import Dict exposing (Dict)
import List.Extra
import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), oneOf, symbol)
import Puzzle exposing (Puzzle)


type alias Dish =
    List (List Field)


type Field
    = Cube
    | Rounded
    | Empty


type alias Error =
    String


calculatePart1 : Dish -> Result Error Int
calculatePart1 =
    List.Extra.transpose
        >> tilt
        >> weight
        >> Ok


calculatePart2 : Dish -> Result Error Int
calculatePart2 =
    List.Extra.transpose
        >> spinCycle 1000000000
        >> weight
        >> Ok


spinCycle : Int -> Dish -> Dish
spinCycle =
    spinCycleHelp Dict.empty


spinCycleHelp : Dict Key Int -> Int -> Dish -> Dish
spinCycleHelp dict spins dish =
    if spins == 0 then
        dish

    else
        let
            cycle =
                tilt >> rotate >> tilt >> rotate >> tilt >> rotate >> tilt >> rotate
        in
        case Dict.get (key dish) dict of
            Just loopEnd ->
                let
                    loopLength =
                        loopEnd - spins
                in
                if spins > loopLength then
                    spinCycleHelp dict (modBy loopLength spins) dish

                else
                    spinCycleHelp dict (spins - 1) (cycle dish)

            Nothing ->
                spinCycleHelp (Dict.insert (key dish) spins dict) (spins - 1) (cycle dish)


type alias Key =
    List (List Int)


key : Dish -> Key
key =
    List.map
        (List.map
            (\field ->
                case field of
                    Rounded ->
                        0

                    Cube ->
                        1

                    Empty ->
                        2
            )
        )


rotate : Dish -> Dish
rotate =
    List.Extra.transpose
        >> List.reverse


tilt : Dish -> Dish
tilt =
    List.map tiltRow


tiltRow : List Field -> List Field
tiltRow fields =
    tiltRowHelp [] fields


tiltRowHelp : List (List Field) -> List Field -> List Field
tiltRowHelp revFragments fields =
    let
        tiltFragment fragment =
            fragment
                |> List.filter ((==) Rounded)
                |> List.length
                |> (\roundedCount ->
                        List.repeat roundedCount Rounded ++ List.repeat (List.length fragment - roundedCount) Empty
                   )
    in
    case List.Extra.splitWhen ((==) Cube) fields of
        Nothing ->
            (tiltFragment fields :: revFragments)
                |> List.reverse
                |> List.intersperse [ Cube ]
                |> List.concat

        Just ( head, remainder ) ->
            tiltRowHelp (tiltFragment head :: revFragments) (List.drop 1 remainder)


weight : Dish -> Int
weight =
    List.map weightRow
        >> List.sum


weightRow : List Field -> Int
weightRow fields =
    fields
        |> List.indexedMap
            (\i field ->
                case field of
                    Rounded ->
                        List.length fields - i

                    _ ->
                        0
            )
        |> List.sum


parser : Parser Dish
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
                [ Parser.succeed Rounded
                    |. symbol "O"
                , Parser.succeed Cube
                    |. symbol "#"
                , Parser.succeed Empty
                    |. symbol "."
                ]
        , trailing = Optional
        }


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
