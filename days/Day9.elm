module Day9 exposing (Mode(..), calculatePart1, calculatePart2, derive, difference, historyParser, parser, puzzle)

import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), andThen, int, loop, map, oneOf, problem, succeed, symbol)
import Puzzle exposing (Puzzle)


type alias History =
    List Int


type Mode
    = Past
    | Future


calculatePart1 : String -> Result (List Parser.DeadEnd) Int
calculatePart1 =
    Parser.run parser
        >> Result.map (List.map (derive Future []) >> List.map List.sum >> List.sum)


calculatePart2 : String -> Result (List Parser.DeadEnd) Int
calculatePart2 =
    Parser.run parser
        >> Result.map (List.map (derive Past []) >> List.map alternatingDifference >> List.sum)


alternatingDifference : List Int -> Int
alternatingDifference =
    List.foldl (\y result -> y - result) 0


derive : Mode -> List Int -> History -> List Int
derive mode ys xs =
    if List.all ((==) 0) xs then
        ys

    else
        let
            y =
                case mode of
                    Future ->
                        List.reverse xs |> List.head |> Maybe.withDefault 0

                    Past ->
                        xs |> List.head |> Maybe.withDefault 0
        in
        derive mode (y :: ys) (difference xs)


difference : List Int -> List Int
difference =
    List.foldl
        (\x acc ->
            case acc.prev of
                Nothing ->
                    { acc | prev = Just x }

                Just prev ->
                    { acc | ys = acc.ys ++ [ x - prev ], prev = Just x }
        )
        { prev = Nothing, ys = [] }
        >> .ys


parser : Parser (List History)
parser =
    loop [] parserHelp


parserHelp : List History -> Parser (Step (List History) (List History))
parserHelp revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= historyParser
        , succeed (Loop revStmts)
            |. symbol "\n"
        , succeed (Loop revStmts)
            |. symbol " "
        , succeed ()
            |> map (\_ -> Done (List.reverse revStmts))
        ]


historyParser : Parser History
historyParser =
    loop [] historyParserHelp


historyParserHelp : History -> Parser (Step History History)
historyParserHelp revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= int
            |. Parser.chompWhile (\c -> c == ' ')
        , succeed identity
            |. symbol "-"
            |= int
            |. Parser.chompWhile (\c -> c == ' ')
            |> map (\stmt -> Loop (negate stmt :: revStmts))
        , succeed ()
            |> andThen
                (\_ ->
                    case revStmts of
                        [] ->
                            problem "no ints here"

                        _ ->
                            succeed ()
                )
            |> map (\_ -> Done <| List.reverse revStmts)
        ]


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = calculatePart1 >> Result.mapError Parser.deadEndsToString
    , calculatePart2 = calculatePart2 >> Result.mapError Parser.deadEndsToString
    }
