module Day15 exposing (Op(..), Step, calculatePart1, calculatePart2, parser, puzzle)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Trailing(..), chompWhile, getChompedString, int, oneOf, spaces, succeed, symbol)
import Puzzle exposing (Puzzle)


type alias Step =
    { label : String
    , op : Op
    }


type Op
    = Put Int
    | Remove


type alias Error =
    String


calculatePart1 : List Step -> Result Error Int
calculatePart1 =
    let
        format step =
            step.label
                ++ (case step.op of
                        Remove ->
                            "-"

                        Put int ->
                            "=" ++ String.fromInt int
                   )
    in
    List.map format >> List.map hash >> List.sum >> Ok


calculatePart2 : List Step -> Result Error Int
calculatePart2 =
    List.foldl install Dict.empty
        >> Dict.toList
        >> List.map boxValue
        >> List.sum
        >> Ok


boxValue : ( Int, Box ) -> Int
boxValue ( boxId, box ) =
    box
        |> List.indexedMap (\i lens -> (boxId + 1) * (i + 1) * lens.focalLength)
        |> List.sum


type alias Box =
    List Lens


type alias Lens =
    { label : String, focalLength : Int }


install : Step -> Dict Int Box -> Dict Int Box
install step =
    let
        boxId =
            hash step.label
    in
    update boxId
        (\box ->
            case step.op of
                Put focalLength ->
                    put { label = step.label, focalLength = focalLength } box

                Remove ->
                    box |> List.filter (\lens -> lens.label /= step.label)
        )


put : Lens -> Box -> Box
put newLens box =
    let
        hasLensLabel =
            box |> List.map .label |> List.member newLens.label
    in
    if hasLensLabel then
        box
            |> List.map
                (\boxLens ->
                    if boxLens.label == newLens.label then
                        newLens

                    else
                        boxLens
                )

    else
        List.append box [ newLens ]


update : Int -> (Box -> Box) -> Dict Int Box -> Dict Int Box
update boxId fn =
    Dict.update boxId
        (\maybeBox ->
            case maybeBox of
                Nothing ->
                    Just (fn [])

                Just box ->
                    Just (fn box)
        )


hash : String -> Int
hash =
    String.toList
        >> List.foldl
            (\char value ->
                value
                    |> ((+) <| Char.toCode char)
                    |> (*) 17
                    |> remainderBy 256
            )
            0


parser : Parser (List Step)
parser =
    Parser.sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = spaces
        , item =
            succeed Step
                |= labelParser
                |= oneOf
                    [ succeed Put
                        |. symbol "="
                        |= int
                    , succeed Remove
                        |. symbol "-"
                    ]
        , trailing = Optional
        }


labelParser : Parser String
labelParser =
    getChompedString <|
        succeed ()
            |. chompWhile Char.isAlpha


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
