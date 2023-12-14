module Day11 exposing (..)

import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), andThen, loop, map, oneOf, succeed, symbol)
import Puzzle exposing (Puzzle)
import Set exposing (Set)


type alias Image =
    Set Position


type alias Position =
    ( Int, Int )


type alias Error =
    String


calculatePart1 : String -> Result Error Int
calculatePart1 input =
    Parser.run parser input
        |> Result.mapError Parser.deadEndsToString
        |> Result.map expand
        |> Result.map sumDistances


sumDistances : Image -> Int
sumDistances =
    pairs
        >> Set.toList
        >> List.map distance
        >> List.sum


distance : ( Position, Position ) -> Int
distance ( pos1, pos2 ) =
    abs (Tuple.first pos1 - Tuple.first pos2)
        + abs (Tuple.second pos1 - Tuple.second pos2)


pairs : Set Position -> Set ( Position, Position )
pairs positions =
    pairsHelp [] (positions |> Set.toList)
        |> Set.fromList


pairsHelp : List ( Position, Position ) -> List Position -> List ( Position, Position )
pairsHelp pairs_ positions =
    case positions of
        head :: tail ->
            pairsHelp ((tail |> List.map (\pos -> ( head, pos ))) ++ pairs_) tail

        [] ->
            pairs_


expand : Image -> Image
expand =
    expandHelp 0 Tuple.first Tuple.mapFirst
        >> expandHelp 0 Tuple.second Tuple.mapSecond


expandHelp : Int -> (Position -> Int) -> ((Int -> Int) -> Position -> Position) -> Image -> Image
expandHelp i extract mod image =
    if Set.filter (\pos -> extract pos >= i) image == Set.empty then
        image

    else
        let
            ( i_, expandedImage ) =
                if Set.filter (\pos -> extract pos == i) image == Set.empty then
                    ( i + 2
                    , image
                        |> Set.map
                            (\pos ->
                                if extract pos > i then
                                    mod ((+) 1) pos

                                else
                                    pos
                            )
                    )

                else
                    ( i + 1, image )
        in
        expandHelp i_ extract mod expandedImage


parser : Parser Image
parser =
    loop ( 0, Set.empty ) parserHelp


parserHelp : ( Int, Image ) -> Parser (Step ( Int, Image ) Image)
parserHelp ( y, positions ) =
    oneOf
        [ succeed (\rowPositions -> Loop ( y + 1, Set.union rowPositions positions ))
            |= rowParser y
        , succeed (Loop ( y, positions ))
            |. symbol "\n"
        , succeed (Loop ( y, positions ))
            |. symbol " "
        , succeed ()
            |> map (\_ -> Done positions)
        ]


rowParser : Int -> Parser (Set Position)
rowParser y =
    loop ( 0, Set.empty ) (rowParserHelp y)


rowParserHelp : Int -> ( Int, Set Position ) -> Parser (Step ( Int, Set Position ) (Set Position))
rowParserHelp y ( x, positions ) =
    oneOf
        [ succeed (Loop ( x + 1, Set.insert ( x, y ) positions ))
            |. symbol "#"
            |. Parser.chompWhile (\c -> c == ' ')
        , succeed (Loop ( x + 1, positions ))
            |. symbol "."
            |. Parser.chompWhile (\c -> c == ' ')
        , succeed ()
            |> andThen
                (\_ ->
                    if x < 1 then
                        Parser.problem "no image part parsed"

                    else
                        Parser.succeed ()
                )
            |> map (\_ -> Done positions)
        ]


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = calculatePart1
    , calculatePart2 = \_ -> Err "not implemented"
    }
