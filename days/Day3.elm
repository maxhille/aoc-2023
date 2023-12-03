module Day3 exposing (calculatePart1, calculatePart2, main)

import Html exposing (text)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Trailing(..)
        , chompWhile
        , int
        , oneOf
        , sequence
        , succeed
        , symbol
        )
import Ui exposing (Ui, ui)


type alias Part =
    { id : Int
    , start : Position
    , end : Position
    }


type alias Position =
    { row : Int
    , col : Int
    }


type alias Symbol =
    { position : Position
    , value : Char
    }


type alias Schematic =
    { symbols : List Symbol
    , parts : List Part
    }


type SymbolPart
    = ParsedSymbol Symbol
    | ParsedPart Part


calculatePart1 : String -> Result (List Parser.DeadEnd) Int
calculatePart1 string =
    substitute ',' string
        |> runParser
        |> Result.map sumParts


calculatePart2 : String -> Result (List Parser.DeadEnd) Int
calculatePart2 string =
    substitute ',' string
        |> runParser
        |> Result.map gearRatios


sumParts : Schematic -> Int
sumParts schematic =
    List.foldr
        (\part sum ->
            if List.map (adjacent part) schematic.symbols |> List.any identity then
                sum + part.id

            else
                sum
        )
        0
        schematic.parts


gearRatios : Schematic -> Int
gearRatios schematic =
    let
        gears =
            schematic.symbols
                |> List.filter (\symbol -> symbol.value == '*')
    in
    List.foldr
        (\gear sum ->
            case List.filter (\part -> adjacent part gear) schematic.parts of
                part1 :: part2 :: _ ->
                    part1.id * part2.id + sum

                _ ->
                    sum
        )
        0
        gears


adjacent : Part -> Symbol -> Bool
adjacent part symbol =
    List.member (part.start.row - symbol.position.row) [ -1, 0, 1 ]
        && (part.start.col <= symbol.position.col + 1)
        && (part.end.col >= symbol.position.col)


substitute : Char -> String -> String
substitute sub =
    String.replace "." (String.fromChar sub)


parser : Char -> Parser Schematic
parser sub =
    sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces =
            chompWhile (\c -> c == ' ' || c == sub || c == '\n')
        , item = symbolPartParser
        , trailing = Optional
        }
        |> Parser.map toSchematic


runParser : String -> Result (List Parser.DeadEnd) Schematic
runParser =
    substitute ',' >> Parser.run (parser ',')


toSchematic : List SymbolPart -> Schematic
toSchematic =
    List.foldr
        (\symbolPart schematic ->
            case symbolPart of
                ParsedSymbol symbol ->
                    { schematic | symbols = symbol :: schematic.symbols }

                ParsedPart part ->
                    { schematic | parts = part :: schematic.parts }
        )
        { symbols = [], parts = [] }


symbolPartParser : Parser SymbolPart
symbolPartParser =
    oneOf
        [ Parser.map ParsedSymbol symbolParser
        , Parser.map ParsedPart partParser
        ]


partParser : Parser Part
partParser =
    succeed (\start id end -> { id = id, start = start, end = end })
        |= getPosition
        |= int
        |= getPosition


getPosition : Parser Position
getPosition =
    Parser.map (\( row, col ) -> { row = row, col = col }) Parser.getPosition


symbolParser : Parser Symbol
symbolParser =
    succeed Symbol
        |= getPosition
        |= schematicSymbol


schematicSymbol : Parser Char
schematicSymbol =
    let
        values =
            [ '*', '#', '+', '$', '-', '/', '=', '%', '@', '&' ]
    in
    oneOf <|
        List.map
            (\value -> Parser.map (\_ -> value) (symbol <| String.fromChar value))
            values


main : Ui
main =
    ui
        [ { title = "Part 1"
          , view =
                calculatePart1
                    >> (\result ->
                            case result of
                                Ok int ->
                                    text <| String.fromInt int

                                Err error ->
                                    text <| Parser.deadEndsToString error
                       )
          }
        , { title = "Part 2"
          , view =
                calculatePart2
                    >> (\result ->
                            case result of
                                Ok int ->
                                    text <| String.fromInt int

                                Err error ->
                                    text <| Parser.deadEndsToString error
                       )
          }
        ]
