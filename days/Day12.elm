module Day12 exposing (Spring(..), calculatePart1, calculatePart2, expand, parser, puzzle, recordParser)

import Dict exposing (Dict)
import List.Extra exposing (intercalate)
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
        |> Result.map (List.map countWithTries)
        |> Result.map List.sum


calculatePart2 : String -> Result Error Int
calculatePart2 input =
    Parser.run parser input
        |> Result.mapError Parser.deadEndsToString
        |> Result.map (List.map expand)
        |> Result.map (List.map countWithTries)
        |> Result.map List.sum


expand : Record -> Record
expand record =
    { springs = record.springs |> List.repeat 5 |> intercalate [ Unknown ]
    , groups = record.groups |> List.repeat 5 |> List.concat
    }


type alias RecordTry =
    { springs : List Spring
    , groups : List Int
    , grouping : Grouping
    }


type Grouping
    = CanStartGroup
    | InGroup
    | Closed


countWithTries : Record -> Int
countWithTries { springs, groups } =
    countWithTriesHelp Dict.empty
        { springs = springs
        , groups = groups
        , grouping = CanStartGroup
        }
        |> Tuple.second


type alias Key =
    ( List Int, List Int, Int )


groupingToInt : Grouping -> Int
groupingToInt grouping =
    case grouping of
        CanStartGroup ->
            0

        InGroup ->
            1

        Closed ->
            2


springsToInt : List Spring -> List Int
springsToInt =
    List.map
        (\spring ->
            case spring of
                Damaged ->
                    0

                Working ->
                    1

                Unknown ->
                    2
        )


key : RecordTry -> Key
key record =
    ( springsToInt record.springs, record.groups, groupingToInt record.grouping )


countWithTriesHelp : Dict Key Int -> RecordTry -> ( Dict Key Int, Int )
countWithTriesHelp dict record =
    case Dict.get (key record) dict of
        Just int ->
            ( dict, int )

        Nothing ->
            (\( resultDict, result ) -> ( Dict.insert (key record) result resultDict, result )) <|
                case reduce record of
                    Completed ->
                        ( dict, 1 )

                    Impossible ->
                        ( dict, 0 )

                    Continue reducedRecord ->
                        let
                            ( damagedDict, damagedResult ) =
                                case tryDamaged reducedRecord of
                                    Completed ->
                                        ( dict, 1 )

                                    Impossible ->
                                        ( dict, 0 )

                                    Continue continueRecord ->
                                        countWithTriesHelp dict continueRecord

                            ( workingDict, workingResult ) =
                                case tryWorking reducedRecord of
                                    Completed ->
                                        ( damagedDict, 1 )

                                    Impossible ->
                                        ( damagedDict, 0 )

                                    Continue continueRecord ->
                                        countWithTriesHelp damagedDict continueRecord
                        in
                        ( workingDict, damagedResult + workingResult )


type Try
    = Completed
    | Impossible
    | Continue RecordTry


reduce : RecordTry -> Try
reduce record =
    case ( record.springs, record.groups ) of
        ( spring :: springs, [] ) ->
            case spring of
                Unknown ->
                    reduce
                        { springs = springs
                        , groups = []
                        , grouping = CanStartGroup
                        }

                Damaged ->
                    Impossible

                Working ->
                    reduce
                        { springs = springs
                        , groups = []
                        , grouping = CanStartGroup
                        }

        ( spring :: springs, group :: groups ) ->
            case spring of
                Unknown ->
                    Continue record

                Damaged ->
                    if record.grouping == Closed then
                        Impossible

                    else if group - 1 == 0 then
                        reduce
                            { springs = springs
                            , groups = groups
                            , grouping = Closed
                            }

                    else
                        reduce
                            { springs = springs
                            , groups = (group - 1) :: groups
                            , grouping = InGroup
                            }

                Working ->
                    if record.grouping == InGroup then
                        Impossible

                    else
                        reduce
                            { springs = springs
                            , groups = group :: groups
                            , grouping = CanStartGroup
                            }

        ( [], [] ) ->
            Completed

        ( [], _ :: _ ) ->
            Impossible


tryDamaged : RecordTry -> Try
tryDamaged record =
    case ( record.springs, record.groups ) of
        ( _ :: springs, group :: groups ) ->
            if record.grouping == Closed then
                Impossible

            else if group - 1 == 0 then
                Continue
                    { springs = springs
                    , groups = groups
                    , grouping = Closed
                    }

            else
                Continue
                    { springs = springs
                    , groups = (group - 1) :: groups
                    , grouping = InGroup
                    }

        ( [], [] ) ->
            Completed

        ( [], _ :: _ ) ->
            Impossible

        ( _ :: _, [] ) ->
            Impossible


tryWorking : RecordTry -> Try
tryWorking record =
    if record.grouping == InGroup then
        Impossible

    else
        case ( record.springs, record.groups ) of
            ( _ :: springs, group :: groups ) ->
                Continue
                    { springs = springs
                    , groups = group :: groups
                    , grouping = CanStartGroup
                    }

            ( [], [] ) ->
                Completed

            ( [], _ :: _ ) ->
                Impossible

            ( _ :: _, [] ) ->
                Impossible


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
    , calculatePart2 = calculatePart2
    }
