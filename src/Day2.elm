module Day2 exposing (Color(..), calculate, gameParser, main, parser)

import Calculator exposing (calculator)
import Parser exposing ((|.), (|=), Parser, Trailing(..), int, oneOf, sequence, spaces, succeed, symbol)


type alias Game =
    { id : Int
    , reveals : List Reveal
    }


condition : { red : Int, green : Int, blue : Int }
condition =
    { red = 12, green = 13, blue = 14 }


calculate : String -> Result (List Parser.DeadEnd) Int
calculate input =
    Parser.run parser input
        |> Result.map
            (List.filter isPossible
                >> List.map .id
                >> List.sum
            )


isPossible : Game -> Bool
isPossible game =
    List.all
        (\reveal ->
            reveal.red <= condition.red && reveal.green <= condition.green && reveal.blue <= condition.blue
        )
        game.reveals


type Color
    = Red
    | Green
    | Blue


type alias Count =
    { color : Color
    , amount : Int
    }


type alias Reveal =
    { red : Int
    , green : Int
    , blue : Int
    }


parser : Parser (List Game)
parser =
    sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item = gameParser
        , trailing = Optional
        }


revealsParser : Parser (List Reveal)
revealsParser =
    sequence
        { start = ""
        , separator = ";"
        , end = ""
        , spaces = spaces
        , item = revealParser
        , trailing = Forbidden
        }


revealParser : Parser Reveal
revealParser =
    sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = spaces
        , item = countParser
        , trailing = Forbidden
        }
        |> Parser.andThen
            (succeed
                << List.foldl
                    (\{ color, amount } acc ->
                        case color of
                            Red ->
                                { acc | red = amount }

                            Green ->
                                { acc | green = amount }

                            Blue ->
                                { acc | blue = amount }
                    )
                    { red = 0, green = 0, blue = 0 }
            )


countParser : Parser Count
countParser =
    succeed (\amount color -> { amount = amount, color = color })
        |= int
        |. symbol " "
        |= colorParser


colorParser : Parser Color
colorParser =
    oneOf
        [ succeed Red
            |. symbol "red"
        , succeed Green
            |. symbol "green"
        , succeed Blue
            |. symbol "blue"
        ]


gameParser : Parser Game
gameParser =
    succeed Game
        |. symbol "Game "
        |= int
        |. symbol ": "
        |= revealsParser


main : Program () Calculator.Model Calculator.Msg
main =
    calculator calculate Parser.deadEndsToString
