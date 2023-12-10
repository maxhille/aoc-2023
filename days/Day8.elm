module Day8 exposing
    ( Instruction(..)
    , calculatePart1
    , instructionForStep
    , parser
    , puzzle
    )

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Trailing(..), chompWhile, getChompedString, sequence, spaces, symbol)
import Puzzle exposing (Puzzle)


type alias Network =
    { instructions : List Instruction
    , forks : Dict Node ( Node, Node )
    }


type Instruction
    = Left
    | Right


type alias Node =
    String


type alias Fork =
    ( Node, ( Node, Node ) )


calculatePart1 : String -> Result (List Parser.DeadEnd) Int
calculatePart1 input =
    Parser.run parser input
        |> Result.map (search 0 "AAA")


search : Int -> Node -> Network -> Int
search step node network =
    if node == "ZZZ" then
        step

    else
        let
            instruction =
                instructionForStep step network.instructions
        in
        search (step + 1) (nextNode node instruction network) network


nextNode : Node -> Instruction -> Network -> Node
nextNode current instruction network =
    let
        fork =
            Dict.get current network.forks
                |> Maybe.withDefault ( "ZZZ", "ZZZ" )
    in
    case instruction of
        Left ->
            Tuple.first fork

        Right ->
            Tuple.second fork


instructionForStep : Int -> List Instruction -> Instruction
instructionForStep step instructions =
    let
        index =
            modBy (List.length instructions) step
    in
    List.drop index instructions |> List.head |> Maybe.withDefault Left


parser : Parser Network
parser =
    Parser.succeed Network
        |. spaces
        |= instructionsParser
        |= forksParser


forksParser : Parser (Dict Node ( Node, Node ))
forksParser =
    Parser.succeed Dict.fromList
        |= sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = spaces
            , item = forkParser
            , trailing = Optional
            }


forkParser : Parser Fork
forkParser =
    Parser.succeed (\node left right -> ( node, ( left, right ) ))
        |= nodeParser
        |. spaces
        |. symbol "="
        |. spaces
        |. symbol "("
        |= nodeParser
        |. symbol ","
        |. spaces
        |= nodeParser
        |. symbol ")"


nodeParser : Parser Node
nodeParser =
    getChompedString (chompWhile Char.isAlphaNum)


instructionsParser : Parser (List Instruction)
instructionsParser =
    getChompedString (chompWhile Char.isAlphaNum)
        |> Parser.map String.toList
        |> Parser.andThen
            (Parser.succeed
                << List.filterMap
                    (\char ->
                        case char of
                            'L' ->
                                Just Left

                            'R' ->
                                Just Right

                            _ ->
                                Nothing
                    )
            )


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = calculatePart1 >> Result.mapError Parser.deadEndsToString
    , calculatePart2 = calculatePart1 >> Result.mapError Parser.deadEndsToString
    }
