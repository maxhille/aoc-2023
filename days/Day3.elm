module Day3 exposing (calculate, main)

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
    }


type alias Schematic =
    { symbols : List Symbol
    , parts : List Part
    }


type SymbolPart
    = ParsedSymbol Symbol
    | ParsedPart Part


calculate : String -> Result (List Parser.DeadEnd) Int
calculate string =
    substitute ',' string
        |> runParser
        |> Result.map sumParts


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
        |. schematicSymbol


schematicSymbol : Parser ()
schematicSymbol =
    [ "*", "#", "+", "$", "-", "/", "=", "%", "@", "&" ] |> List.map symbol |> oneOf


main : Ui
main =
    ui
        [ { title = "Part 1"
          , view =
                calculate
                    >> (\result ->
                            case result of
                                Ok int ->
                                    text <| String.fromInt int

                                Err error ->
                                    text <| Parser.deadEndsToString error
                       )
          }
        ]
