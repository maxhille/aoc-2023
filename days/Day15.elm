module Day15 exposing (calculatePart1, calculatePart2, parser, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..), chompUntilEndOr, getChompedString, spaces, succeed)
import Puzzle exposing (Puzzle)


type alias Error =
    String


calculatePart1 : List String -> Result Error Int
calculatePart1 =
    List.map hash >> List.sum >> Ok


calculatePart2 : List String -> Result Error Int
calculatePart2 _ =
    Err "not implemented"


hash : String -> Int
hash =
    String.toList
        >> List.foldl
            (\char value ->
                value
                    |> ((+) <| Char.toCode char)
                    |> (*) 17
                    |> remainderBy 256
            )
            0


parser : Parser (List String)
parser =
    Parser.sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = spaces
        , item =
            getChompedString <|
                succeed ()
                    |. chompUntilEndOr ","
        , trailing = Optional
        }


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
