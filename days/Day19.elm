module Day19 exposing (Condition(..), Split(..), To(..), calculatePart1, calculatePart2, parser, puzzle, split)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Trailing(..), int, oneOf, spaces, succeed, symbol, variable)
import Puzzle exposing (Puzzle)
import Set


type alias System =
    { rules : List Rule
    , parts : List Part
    }


type alias Part =
    { x : Int
    , m : Int
    , a : Int
    , s : Int
    }


type alias PartRange =
    { x : Range
    , m : Range
    , a : Range
    , s : Range
    }


type alias Range =
    { from : Int, to : Int }


type alias Rule =
    { id : String
    , conditions : List Condition
    }


type Condition
    = X Order Int To
    | M Order Int To
    | A Order Int To
    | S Order Int To
    | Default To


type To
    = Accept
    | Reject
    | Next String


calculatePart1 : System -> Result String Int
calculatePart1 system =
    let
        toRange int =
            { from = int, to = int }

        partRanges =
            system.parts
                |> List.map
                    (\{ x, m, a, s } ->
                        { x = toRange x
                        , m = toRange m
                        , a = toRange a
                        , s = toRange s
                        }
                    )
    in
    process system.rules partRanges
        |> Result.map
            (List.map (\{ x, m, a, s } -> x.from + m.from + a.from + s.from)
                >> List.sum
            )


calculatePart2 : System -> Result String Int
calculatePart2 system =
    process system.rules
        [ { x = { from = 1, to = 4000 }
          , m = { from = 1, to = 4000 }
          , a = { from = 1, to = 4000 }
          , s = { from = 1, to = 4000 }
          }
        ]
        |> Result.map
            (List.map (\{ x, m, a, s } -> (x.to - x.from + 1) * (m.to - m.from + 1) * (a.to - a.from + 1) * (s.to - s.from + 1))
                >> List.sum
            )


process : List Rule -> List PartRange -> Result String (List PartRange)
process rules =
    let
        dict =
            rules |> List.map (\rule -> ( rule.id, rule )) |> Dict.fromList
    in
    List.map (\part -> { part = part, to = Next "in" })
        >> processHelp dict


processHelp : Dict String Rule -> List { part : PartRange, to : To } -> Result String (List PartRange)
processHelp rules parts =
    if parts |> List.all (\{ to } -> to == Accept) then
        Ok <| List.map .part parts

    else
        case
            List.foldl
                (\{ part, to } acc ->
                    case acc of
                        Err err ->
                            Err err

                        Ok parts_ ->
                            case to of
                                Reject ->
                                    Ok parts_

                                Accept ->
                                    Ok <| { part = part, to = to } :: parts_

                                Next ruleId ->
                                    case Dict.get ruleId rules of
                                        Nothing ->
                                            Err "could not find rule by ID"

                                        Just rule ->
                                            case apply rule.conditions part of
                                                Ok newParts ->
                                                    Ok <| List.append newParts parts_

                                                Err err ->
                                                    Err err
                )
                (Ok [])
                parts
        of
            Err err ->
                Err err

            Ok parts_ ->
                processHelp rules parts_


apply : List Condition -> PartRange -> Result String (List { part : PartRange, to : To })
apply conditions part =
    case conditions of
        [] ->
            Err "no condition matched"

        [ condition ] ->
            case condition of
                Default to ->
                    Ok <| [ { part = part, to = to } ]

                _ ->
                    Err "last condition was no default"

        condition :: remainder ->
            case condition of
                X order int to ->
                    case split order int part.x of
                        Both toRange nextRange ->
                            Result.map2 (::)
                                (Ok <| { part = { part | x = toRange }, to = to })
                                (apply remainder { part | x = nextRange })

                        OnlyTo toRange ->
                            Ok <| [ { part = { part | x = toRange }, to = to } ]

                        OnlyNext nextRange ->
                            apply remainder { part | x = nextRange }

                M order int to ->
                    case split order int part.m of
                        Both toRange nextRange ->
                            Result.map2 (::)
                                (Ok <| { part = { part | m = toRange }, to = to })
                                (apply remainder { part | m = nextRange })

                        OnlyTo toRange ->
                            Ok <| [ { part = { part | m = toRange }, to = to } ]

                        OnlyNext nextRange ->
                            apply remainder { part | m = nextRange }

                A order int to ->
                    case split order int part.a of
                        Both toRange nextRange ->
                            Result.map2 (::)
                                (Ok <| { part = { part | a = toRange }, to = to })
                                (apply remainder { part | a = nextRange })

                        OnlyTo toRange ->
                            Ok <| [ { part = { part | a = toRange }, to = to } ]

                        OnlyNext nextRange ->
                            apply remainder { part | a = nextRange }

                S order int to ->
                    case split order int part.s of
                        Both toRange nextRange ->
                            Result.map2 (::)
                                (Ok <| { part = { part | s = toRange }, to = to })
                                (apply remainder { part | s = nextRange })

                        OnlyTo toRange ->
                            Ok <| [ { part = { part | s = toRange }, to = to } ]

                        OnlyNext nextRange ->
                            apply remainder { part | s = nextRange }

                Default to ->
                    Ok <| [ { part = part, to = to } ]


type Split
    = Both Range Range
    | OnlyNext Range
    | OnlyTo Range


split : Order -> Int -> Range -> Split
split order int range =
    if order == GT then
        if range.from > int then
            OnlyTo range

        else if range.to <= int then
            OnlyNext range

        else
            Both { from = int + 1, to = range.to } { from = range.from, to = int }

    else if range.to < int then
        OnlyTo range

    else if range.from >= int then
        OnlyNext range

    else
        Both { from = range.from, to = int - 1 } { from = int, to = range.to }



-- compare part.x int == order then


parser : Parser System
parser =
    succeed System
        |= rulesParser
        |. spaces
        |= partsParser


rulesParser : Parser (List Rule)
rulesParser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item =
            succeed Rule
                |= ruleIdParser
                |= conditionsParser
        , trailing = Optional
        }


conditionsParser : Parser (List Condition)
conditionsParser =
    Parser.sequence
        { start = "{"
        , separator = ","
        , end = "}"
        , spaces = spaces
        , item = conditionParser
        , trailing = Optional
        }


conditionParser : Parser Condition
conditionParser =
    let
        symbols =
            [ ( X, "x" )
            , ( M, "m" )
            , ( A, "a" )
            , ( S, "s" )
            ]
    in
    oneOf <|
        List.map
            (\( constructor, symbol_ ) ->
                succeed constructor
                    |. Parser.backtrackable (symbol symbol_)
                    |= oneOf
                        [ succeed GT
                            |. symbol ">"
                        , succeed LT
                            |. symbol "<"
                        ]
                    |= int
                    |. symbol ":"
                    |= evaluationParser
            )
            symbols
            ++ [ succeed Default
                    |= evaluationParser
               ]


evaluationParser : Parser To
evaluationParser =
    oneOf
        [ succeed Accept
            |. symbol "A"
        , succeed Reject
            |. symbol "R"
        , succeed Next
            |= ruleIdParser
        ]


ruleIdParser : Parser String
ruleIdParser =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlpha c
        , reserved = Set.empty
        }


partsParser : Parser (List Part)
partsParser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item =
            succeed Part
                |. symbol "{x="
                |= int
                |. symbol ",m="
                |= int
                |. symbol ",a="
                |= int
                |. symbol ",s="
                |= int
                |. symbol "}"
        , trailing = Optional
        }


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
