module Day7 exposing
    ( Hand(..)
    , Jokers(..)
    , Strength(..)
    , calculatePart1
    , calculatePart2
    , entryParser
    , puzzle
    , strength
    )

import Dict
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


type Jokers
    = With
    | Without


calculatePart1 : String -> Result (List Parser.DeadEnd) Int
calculatePart1 =
    calculate Without


calculatePart2 : String -> Result (List Parser.DeadEnd) Int
calculatePart2 =
    calculate With


calculate : Jokers -> String -> Result (List Parser.DeadEnd) Int
calculate jokers input =
    Parser.run parser input
        |> Result.map
            (\entries ->
                entries
                    |> List.sortWith (compareEntries jokers)
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


compareEntries : Jokers -> Entry -> Entry -> Order
compareEntries jokers entry1 entry2 =
    compareHand jokers entry1.hand entry2.hand


compareHand : Jokers -> Hand -> Hand -> Order
compareHand jokers ((Hand a1 b1 c1 d1 e1) as hand1) ((Hand a2 b2 c2 d2 e2) as hand2) =
    case compareStrength (strength jokers hand1) (strength jokers hand2) of
        EQ ->
            let
                order1 =
                    [ a1, b1, c1, d1, e1 ] |> List.map (cardOrder jokers)

                order2 =
                    [ a2, b2, c2, d2, e2 ] |> List.map (cardOrder jokers)
            in
            compare order1 order2

        LT ->
            LT

        GT ->
            GT


cardOrder : Jokers -> Char -> Int
cardOrder jokers char =
    case char of
        'A' ->
            0

        'K' ->
            -1

        'Q' ->
            -2

        'J' ->
            case jokers of
                Without ->
                    -3

                With ->
                    -100

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


strength : Jokers -> Hand -> Strength
strength jokers (Hand a b c d e) =
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
        |> (\dict ->
                case jokers of
                    Without ->
                        ( dict, 0 )

                    With ->
                        let
                            jokerCount =
                                Dict.get 'J' dict |> Maybe.withDefault 0
                        in
                        ( Dict.update 'J' (\_ -> Just 0) dict, jokerCount )
           )
        |> (Tuple.mapFirst <| Dict.values >> List.sort >> List.reverse)
        |> (\( counts, jokerCount ) ->
                case counts of
                    head :: tail ->
                        (head + jokerCount) :: tail

                    [] ->
                        []
           )
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
    , calculatePart2 = calculatePart2 >> Result.mapError Parser.deadEndsToString
    }
