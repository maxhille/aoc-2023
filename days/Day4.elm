module Day4 exposing (calculatePart1, calculatePart2, main, parser)

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


calculatePart2 : String -> Result (List Parser.DeadEnd) Int
calculatePart2 input =
    Parser.run parser input
        |> Result.map
            (\cards ->
                List.foldl
                    (\card acc ->
                        let
                            remaining =
                                List.drop 1 acc.remaining
                        in
                        { acc
                            | remaining = remaining
                            , resulting = copies card remaining ++ acc.resulting
                        }
                    )
                    { remaining = cards
                    , resulting = []
                    }
                    cards
                    |> .resulting
                    |> List.length
            )


copies : Card -> List Card -> List Card
copies card remainder =
    card
        :: (matches card
                |> List.range 1
                |> List.map (\int -> ( List.drop (int - 1) remainder |> List.head, List.drop int remainder ))
                |> List.filterMap (\( maybeHead, copyRemainder ) -> Maybe.map (\justHead -> ( justHead, copyRemainder )) maybeHead)
                |> List.map (\( head, remainer ) -> copies head remainer)
                |> List.concat
           )


matches : Card -> Int
matches card =
    Set.intersect (card.winning |> Set.fromList) (card.yours |> Set.fromList)
        |> Set.size


score : Card -> Int
score card =
    matches card
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
