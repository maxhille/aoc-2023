module Day18 exposing (Color(..), Direction(..), calculatePart1, calculatePart2, parser, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..), int, oneOf, spaces, succeed, symbol)
import Puzzle exposing (Puzzle)
import Set exposing (Set)


type alias Plan =
    List Instruction


type alias Lagoon =
    Set Position


type alias Instruction =
    { direction : Direction
    , distance : Int
    , color : Color
    }


type Color
    = Color Int Int Int


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Position =
    ( Int, Int )


type alias Error =
    String


calculatePart1 : Plan -> Result Error Int
calculatePart1 =
    dig >> Set.size >> Ok


calculatePart2 : Plan -> Result Error Int
calculatePart2 _ =
    Err "not implemented"


dig : Plan -> Lagoon
dig =
    trench >> interior


trench : Plan -> Set Position
trench =
    trenchHelp [ ( 0, 0 ) ] >> Set.fromList


trenchHelp : List Position -> Plan -> List Position
trenchHelp revPositions plan =
    case ( revPositions, plan ) of
        ( lastPos :: _, instruction :: remainder ) ->
            trenchHelp (List.append (move lastPos instruction) revPositions) remainder

        ( _, _ ) ->
            revPositions


move : Position -> Instruction -> List Position
move ( x, y ) { direction, distance } =
    let
        range =
            List.range 1 distance |> List.reverse
    in
    case direction of
        Up ->
            List.map (\dy -> ( x, y - dy )) range

        Down ->
            List.map (\dy -> ( x, y + dy )) range

        Left ->
            List.map (\dx -> ( x - dx, y )) range

        Right ->
            List.map (\dx -> ( x + dx, y )) range


interior : Set Position -> Set Position
interior bounds =
    case firstInside bounds of
        Just position ->
            interiorHelp [ position ] bounds

        Nothing ->
            bounds


interiorHelp : List Position -> Set Position -> Set Position
interiorHelp queue positions =
    case queue of
        [] ->
            positions

        head :: tail ->
            if Set.member head positions then
                interiorHelp tail positions

            else
                let
                    neighbours ( x, y ) =
                        [ ( x - 1, y ), ( x, y + 1 ), ( x + 1, y ), ( x, y - 1 ) ]
                in
                interiorHelp (List.append tail (neighbours head)) (Set.insert head positions)


firstInside : Set Position -> Maybe Position
firstInside =
    -- there are some degenerate maps where this is not general enough
    Set.partition (\( _, y ) -> y == 1)
        >> Tuple.first
        >> Set.toList
        >> List.sortBy Tuple.first
        >> List.head
        >> Maybe.map (\( x, y ) -> ( x + 1, y ))


parser : Parser Plan
parser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item =
            succeed Instruction
                |= directionParser
                |. spaces
                |= int
                |. spaces
                |= colorParser
        , trailing = Optional
        }


colorParser : Parser Color
colorParser =
    succeed Color
        |. symbol "("
        |. symbol "#"
        |= byteParser
        |= byteParser
        |= byteParser
        |. symbol ")"


byteParser : Parser Int
byteParser =
    succeed (\a b -> a * 16 + b)
        |= halfByteParser
        |= halfByteParser


halfByteParser : Parser Int
halfByteParser =
    let
        hexChars =
            [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f" ]
    in
    oneOf <|
        List.indexedMap (\i char -> succeed i |. symbol char) hexChars


directionParser : Parser Direction
directionParser =
    oneOf
        [ succeed Up
            |. symbol "U"
        , succeed Right
            |. symbol "R"
        , succeed Down
            |. symbol "D"
        , succeed Left
            |. symbol "L"
        ]


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
