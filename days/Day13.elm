module Day13 exposing (Field(..), calculatePart1, horizontalReflection, parser, patternParser, puzzle)

import List.Extra
import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), andThen, loop, map, oneOf, spaces, succeed, symbol)
import Puzzle exposing (Puzzle)


type alias Pattern =
    List (List Field)


type Field
    = Ash
    | Rocks


type alias Error =
    String


calculatePart1 : List Pattern -> Result Error Int
calculatePart1 =
    List.map resultForPattern >> joinResults >> Result.map List.sum


resultForPattern : Pattern -> Result Error Int
resultForPattern pattern =
    case ( horizontalReflection pattern, verticalReflection pattern ) of
        ( Just row, Nothing ) ->
            Ok <| row * 100

        ( Nothing, Just col ) ->
            Ok col

        ( Nothing, Nothing ) ->
            Err "Zero reflections found."

        ( Just _, Just _ ) ->
            Err "Two reflections found."


verticalReflection : Pattern -> Maybe Int
verticalReflection =
    List.Extra.transpose >> horizontalReflection


horizontalReflection : Pattern -> Maybe Int
horizontalReflection pattern =
    List.map (\i -> ( i, isReflection pattern i )) (List.range 1 (List.length pattern - 1))
        |> List.filter (\( _, valid ) -> valid)
        |> List.map Tuple.first
        |> List.head


isReflection : Pattern -> Int -> Bool
isReflection pattern row =
    let
        rows1 =
            pattern |> List.take row |> List.reverse

        rows2 =
            pattern |> List.drop row

        maybeMinlength =
            [ rows1, rows2 ]
                |> List.map List.length
                |> List.minimum
    in
    case maybeMinlength of
        Just minLength ->
            List.take minLength rows1 == List.take minLength rows2

        Nothing ->
            False


parser : Parser (List Pattern)
parser =
    loop [] parserHelp


parserHelp : List Pattern -> Parser (Step (List Pattern) (List Pattern))
parserHelp revPatterns =
    oneOf
        [ succeed (\pattern -> Loop (pattern :: revPatterns))
            |= patternParser
            |. spaces
        , succeed ()
            |> map (\_ -> Done <| List.reverse revPatterns)
        ]


patternParser : Parser Pattern
patternParser =
    loop [] patternParserHelp


patternParserHelp : Pattern -> Parser (Step Pattern Pattern)
patternParserHelp revPattern =
    oneOf
        [ succeed (\line -> Loop <| line :: revPattern)
            |= fieldsParser
            |. oneOf
                [ Parser.end
                , Parser.symbol "\n"
                ]
        , succeed ()
            |> andThen
                (\_ ->
                    if revPattern == [] then
                        Parser.problem "no pattern parsed"

                    else
                        Parser.succeed (Done <| List.reverse revPattern)
                )
        ]


fieldsParser : Parser (List Field)
fieldsParser =
    loop [] fieldsParserHelp


fieldsParserHelp : List Field -> Parser (Step (List Field) (List Field))
fieldsParserHelp revFields =
    oneOf
        [ succeed (Loop <| Ash :: revFields)
            |. symbol "."
        , succeed (Loop <| Rocks :: revFields)
            |. symbol "#"
        , succeed ()
            |> andThen
                (\_ ->
                    if revFields == [] then
                        Parser.problem "no fields parsed"

                    else
                        Parser.succeed (Done <| List.reverse revFields)
                )
        ]


joinResults : List (Result Error a) -> Result Error (List a)
joinResults =
    List.foldr
        (\result acc ->
            case acc of
                Err _ ->
                    acc

                Ok as_ ->
                    case result of
                        Err err ->
                            Err err

                        Ok a ->
                            Ok <| a :: as_
        )
        (Ok [])


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = \_ -> Err "not implemented"
    }
