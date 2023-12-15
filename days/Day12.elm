module Day12 exposing (..)

import List.Extra exposing (elemIndex, setAt, updateAt)
import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), andThen, int, loop, map, oneOf, succeed, symbol)
import Puzzle exposing (Puzzle)


type alias Record =
    { springs : List Spring
    , groups : List Int
    }


type Spring
    = Working
    | Damaged
    | Unknown


type alias Error =
    String


calculatePart1 : String -> Result Error Int
calculatePart1 input =
    Parser.run parser input
        |> Result.mapError Parser.deadEndsToString
        |> Result.map (List.map countArrangements)
        |> Result.map List.sum


countArrangements : Record -> Int
countArrangements record =
    countArrangementsHelp [ record ]


countArrangementsHelp : List Record -> Int
countArrangementsHelp records =
    if records |> List.any (.springs >> List.member Unknown) |> not then
        records
            |> List.filter valid
            |> List.length

    else
        let
            newRecords =
                List.foldl
                    (\record acc ->
                        let
                            record1 =
                                replaceFirstUnkownWith Working record

                            record2 =
                                replaceFirstUnkownWith Damaged record
                        in
                        record1 :: record2 :: acc
                    )
                    []
                    records
        in
        countArrangementsHelp newRecords


valid : Record -> Bool
valid record =
    record.springs
        |> List.foldl
            (\spring acc ->
                case spring of
                    Damaged ->
                        case acc.aggregating of
                            Just count ->
                                { acc | aggregating = Just <| count + 1 }

                            Nothing ->
                                { acc | aggregating = Just 1 }

                    _ ->
                        case acc.aggregating of
                            Just count ->
                                { acc
                                    | groups = count :: acc.groups
                                    , aggregating = Nothing
                                }

                            Nothing ->
                                acc
            )
            { aggregating = Nothing, groups = [] }
        |> (\{ aggregating, groups } ->
                case aggregating of
                    Just count ->
                        count :: groups

                    Nothing ->
                        groups
           )
        |> List.reverse
        |> (==) record.groups


replaceFirstUnkownWith : Spring -> Record -> Record
replaceFirstUnkownWith spring record =
    let
        maybeIndex =
            elemIndex Unknown record.springs

        updatedSprings =
            case maybeIndex of
                Just i ->
                    setAt i spring record.springs

                Nothing ->
                    record.springs
    in
    { record | springs = updatedSprings }


parser : Parser (List Record)
parser =
    loop [] parserHelp


parserHelp : List Record -> Parser (Step (List Record) (List Record))
parserHelp revRecords =
    oneOf
        [ succeed (Loop revRecords)
            |. symbol "\n"
        , succeed (Loop revRecords)
            |. symbol " "
        , succeed (\record -> Loop (record :: revRecords))
            |= recordParser
        , succeed ()
            |> map (\_ -> Done <| List.reverse revRecords)
        ]


recordParser : Parser Record
recordParser =
    loop { springs = [], groups = [] } recordParserHelp


recordParserHelp : Record -> Parser (Step Record Record)
recordParserHelp revRecord =
    oneOf
        [ succeed (Loop { revRecord | springs = Damaged :: revRecord.springs })
            |. symbol "#"
        , succeed (Loop { revRecord | springs = Working :: revRecord.springs })
            |. symbol "."
        , succeed (Loop { revRecord | springs = Unknown :: revRecord.springs })
            |. symbol "?"
        , succeed (Loop revRecord)
            |. symbol " "
        , succeed (Loop revRecord)
            |. symbol ","
        , succeed (\i -> Loop { revRecord | groups = i :: revRecord.groups })
            |= int
        , succeed ()
            |> andThen
                (\_ ->
                    if revRecord.springs == [] then
                        Parser.problem "no springs parsed"

                    else
                        Parser.succeed ()
                )
            |> map
                (\_ ->
                    Done
                        { springs = List.reverse revRecord.springs
                        , groups = List.reverse revRecord.groups
                        }
                )
        ]


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = calculatePart1
    , calculatePart2 = \_ -> Err "not implemented"
    }
