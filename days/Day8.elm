module Day8 exposing
    ( Instruction(..)
    , calculatePart1
    , calculatePart2
    , factors
    , instructionForStep
    , lcm
    , parser
    , puzzle
    )

import Dict exposing (Dict)
import List.Extra
import Parser exposing ((|.), (|=), Parser, Trailing(..), chompWhile, getChompedString, sequence, spaces, symbol)
import Puzzle exposing (Puzzle)


type alias Network =
    { instructions : List Instruction
    , forks : Dict Node ( Node, Node )
    }


type Instruction
    = Left
    | Right


type Mode
    = Ghost
    | Human


type alias Node =
    String


type alias Fork =
    ( Node, ( Node, Node ) )


calculatePart1 : String -> Result (List Parser.DeadEnd) Int
calculatePart1 input =
    Parser.run parser input
        |> Result.map (search Human 0 "AAA")


calculatePart2 : String -> Result (List Parser.DeadEnd) Int
calculatePart2 input =
    Parser.run parser input
        |> Result.map
            (\network ->
                findStartNodes network
                    |> List.map (\node -> search Ghost 0 node network)
                    |> lcm
                    -- this step assumes that 1. the instruction cycle lengths are divisors of ghost path lenght and ghost paths always return right to their start point
                    |> Maybe.withDefault 0
            )


lcm : List Int -> Maybe Int
lcm =
    List.map factors
        >> pad
        >> List.Extra.transpose
        >> List.map List.maximum
        >> List.map (Maybe.withDefault 0)
        >> List.foldl
            (\exp acc ->
                { acc
                    | index = acc.index + 1
                    , product = acc.product * acc.index ^ exp
                }
            )
            { product = 1, index = 1 }
        >> .product
        >> Just


pad : List (List Int) -> List (List Int)
pad xss =
    let
        length =
            List.map List.length xss
                |> List.maximum
                |> Maybe.withDefault 0
    in
    List.map (\xs -> xs ++ List.repeat (length - List.length xs) 0) xss


factors : Int -> List Int
factors n =
    1 :: factorsHelper [] (List.range 2 n) n


factorsHelper : List Int -> List Int -> Int -> List Int
factorsHelper fs ps n =
    if n == 1 then
        fs

    else
        case ps of
            pn :: pr ->
                let
                    count =
                        divsBy pn n
                in
                factorsHelper (List.append fs [ count ]) pr (n // (pn ^ count))

            [] ->
                1 :: fs


divsBy : Int -> Int -> Int
divsBy by n =
    if modBy by n == 0 then
        1 + divsBy by (n // by)

    else
        0


findStartNodes : Network -> List Node
findStartNodes =
    .forks
        >> Dict.keys
        >> List.filter (String.endsWith "A")


search : Mode -> Int -> Node -> Network -> Int
search mode step node network =
    if mode == Human && node == "ZZZ" then
        step

    else if mode == Ghost && String.endsWith "Z" node then
        step

    else
        let
            instruction =
                instructionForStep step network.instructions
        in
        search mode (step + 1) (nextNode node instruction network) network


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
    , calculatePart2 = calculatePart2 >> Result.mapError Parser.deadEndsToString
    }
