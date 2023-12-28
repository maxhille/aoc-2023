module Day19 exposing (Condition(..), To(..), calculatePart1, calculatePart2, parser, puzzle)

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
    process system.rules system.parts
        |> Result.map (List.map (\{ x, m, a, s } -> x + m + a + s) >> List.sum)


calculatePart2 : System -> Result String Int
calculatePart2 _ =
    Err "not implemented"


process : List Rule -> List Part -> Result String (List Part)
process rules =
    let
        dict =
            rules |> List.map (\rule -> ( rule.id, rule )) |> Dict.fromList
    in
    List.map (\part -> processHelp dict part (Next "in"))
        >> List.foldr
            (\partResult partsResult ->
                case ( partsResult, partResult ) of
                    ( Ok parts, Ok maybePart ) ->
                        case maybePart of
                            Just part ->
                                Ok <| part :: parts

                            Nothing ->
                                Ok parts

                    ( _, Err error ) ->
                        Err error

                    ( Err error, _ ) ->
                        Err error
            )
            (Ok [])


processHelp : Dict String Rule -> Part -> To -> Result String (Maybe Part)
processHelp rules part to =
    case to of
        Accept ->
            Ok <| Just part

        Reject ->
            Ok Nothing

        Next ruleId ->
            case Dict.get ruleId rules of
                Nothing ->
                    Err "unknown rule ID"

                Just rule ->
                    apply rule.conditions part |> Result.andThen (processHelp rules part)


apply : List Condition -> Part -> Result String To
apply conditions part =
    case conditions of
        [] ->
            Err "no condition matched"

        [ condition ] ->
            case condition of
                Default to ->
                    Ok to

                _ ->
                    Err "last condition was no default"

        condition :: remainder ->
            case condition of
                X order int to ->
                    if compare part.x int == order then
                        Ok to

                    else
                        apply remainder part

                M order int to ->
                    if compare part.m int == order then
                        Ok to

                    else
                        apply remainder part

                A order int to ->
                    if compare part.a int == order then
                        Ok to

                    else
                        apply remainder part

                S order int to ->
                    if compare part.s int == order then
                        Ok to

                    else
                        apply remainder part

                Default to ->
                    Ok to


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
