module Day7 exposing (Hand(..), Strength(..), calculatePart1, entryParser, puzzle, strength)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Trailing(..), chompWhile, getChompedString, int, sequence, spaces)
import Puzzle exposing (Puzzle)


type alias Entry =
    { hand : Hand
    , bid : Int
    }


type Strength
    = FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard


type Hand
    = Hand Char Char Char Char Char


calculatePart1 : String -> Result (List Parser.DeadEnd) Int
calculatePart1 input =
    Parser.run parser input
        |> Result.map
            (\entries ->
                entries
                    |> List.sortWith compareEntries
                    |> List.foldl
                        (\entry { rank, total } ->
                            { rank = rank + 1
                            , total = total + rank * entry.bid
                            }
                        )
                        { rank = 1
                        , total = 0
                        }
                    |> .total
            )


compareEntries : Entry -> Entry -> Order
compareEntries entry1 entry2 =
    compareHand entry1.hand entry2.hand


compareHand : Hand -> Hand -> Order
compareHand ((Hand a1 b1 c1 d1 e1) as hand1) ((Hand a2 b2 c2 d2 e2) as hand2) =
    case compareStrength (strength hand1) (strength hand2) of
        EQ ->
            let
                order1 =
                    [ a1, b1, c1, d1, e1 ] |> List.map cardOrder

                order2 =
                    [ a2, b2, c2, d2, e2 ] |> List.map cardOrder
            in
            compare order1 order2

        LT ->
            LT

        GT ->
            GT


cardOrder : Char -> Int
cardOrder char =
    case char of
        'A' ->
            0

        'K' ->
            -1

        'Q' ->
            -2

        'J' ->
            -3

        'T' ->
            -4

        '9' ->
            -5

        '8' ->
            -6

        '7' ->
            -7

        '6' ->
            -8

        '5' ->
            -9

        '4' ->
            -10

        '3' ->
            -11

        '2' ->
            -12

        _ ->
            -13


compareCard : Char -> Char -> Order
compareCard char1 char2 =
    let
        order char_ =
            case char_ of
                'A' ->
                    0

                'K' ->
                    -1

                'D' ->
                    -2

                'J' ->
                    -3

                'T' ->
                    -4

                '9' ->
                    -5

                '8' ->
                    -6

                '7' ->
                    -7

                '6' ->
                    -8

                '5' ->
                    -9

                '4' ->
                    -10

                '3' ->
                    -11

                '2' ->
                    -12

                _ ->
                    -13
    in
    compare (order char1) (order char2)


compareStrength : Strength -> Strength -> Order
compareStrength strength1 strength2 =
    let
        order strength_ =
            case strength_ of
                FiveOfAKind ->
                    0

                FourOfAKind ->
                    -1

                FullHouse ->
                    -2

                ThreeOfAKind ->
                    -3

                TwoPair ->
                    -4

                OnePair ->
                    -5

                HighCard ->
                    -6
    in
    compare (order strength1) (order strength2)


strength : Hand -> Strength
strength (Hand a b c d e) =
    let
        add char =
            Dict.update char (Maybe.withDefault 0 >> (+) 1 >> Just)
    in
    Dict.empty
        |> add a
        |> add b
        |> add c
        |> add d
        |> add e
        |> Dict.values
        |> (List.sort >> List.reverse)
        |> (\counts ->
                case counts of
                    5 :: _ ->
                        FiveOfAKind

                    4 :: _ ->
                        FourOfAKind

                    3 :: 2 :: _ ->
                        FullHouse

                    3 :: _ ->
                        ThreeOfAKind

                    2 :: 2 :: _ ->
                        TwoPair

                    2 :: _ ->
                        OnePair

                    _ ->
                        HighCard
           )


parser : Parser (List Entry)
parser =
    Parser.succeed identity
        |= sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = spaces
            , item = entryParser
            , trailing = Optional
            }


entryParser : Parser Entry
entryParser =
    Parser.succeed Entry
        |= handParser
        |. spaces
        |= int


handParser : Parser Hand
handParser =
    getChompedString (chompWhile Char.isAlphaNum)
        |> Parser.andThen
            (\str ->
                case String.toList str of
                    a :: b :: c :: d :: e :: _ ->
                        Parser.succeed <| Hand a b c d e

                    _ ->
                        Parser.problem <| "not a Hand: " ++ str
            )


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "Got Sheet") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = calculatePart1 >> Result.mapError Parser.deadEndsToString
    , calculatePart2 = (\_ -> Ok 0) >> Result.mapError Parser.deadEndsToString
    }
