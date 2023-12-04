module Day4 exposing (calculatePart1, main, parser)

import Html exposing (text)
import Parser exposing ((|.), (|=), Parser, Trailing(..), int, sequence, spaces, succeed, symbol)
import Set
import Ui exposing (Ui, ui)


type alias Card =
    { winning : List Int
    , yours : List Int
    }


calculatePart1 : String -> Result (List Parser.DeadEnd) Int
calculatePart1 input =
    Parser.run parser input
        |> Result.map (List.map score)
        |> Result.map List.sum


score : Card -> Int
score card =
    Set.intersect (card.winning |> Set.fromList) (card.yours |> Set.fromList)
        |> Set.size
        |> (\size ->
                if size == 0 then
                    0

                else
                    2 ^ (size - 1)
           )


parser : Parser (List Card)
parser =
    sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item = cardParser
        , trailing = Optional
        }


cardParser : Parser Card
cardParser =
    succeed Card
        |. symbol "Card"
        |. spaces
        |. int
        |. symbol ":"
        |= numbersParser
        |. symbol "|"
        |= numbersParser


numbersParser : Parser (List Int)
numbersParser =
    sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item = int
        , trailing = Optional
        }


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
        ]
