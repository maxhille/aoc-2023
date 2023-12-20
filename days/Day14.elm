module Day14 exposing (Field(..), calculatePart1, parser, puzzle)

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
        >> List.map tiltRow
        >> List.map weight
        >> List.sum
        >> Ok


calculatePart2 : Dish -> Result Error Int
calculatePart2 _ =
    Err "not implemented"


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


weight : List Field -> Int
weight fields =
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
