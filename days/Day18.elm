module Day18 exposing (Direction(..), calculatePart1, calculatePart2, parser, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..), int, oneOf, problem, spaces, succeed, symbol)
import Puzzle exposing (Puzzle)


type alias Plan =
    List ColoredInstruction


type alias Instruction =
    { direction : Direction
    , distance : Int
    }


type alias ColoredInstruction =
    { direction : Direction
    , distance : Int
    , colorInstruction : Instruction
    }


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
    List.map (\coloredInstruction -> { distance = coloredInstruction.distance, direction = coloredInstruction.direction })
        >> dig
        >> Ok


calculatePart2 : Plan -> Result Error Int
calculatePart2 =
    List.map .colorInstruction
        >> dig
        >> Ok


dig : List Instruction -> Int
dig =
    trench >> area


trench : List Instruction -> List Position
trench =
    trenchHelp [ ( 0, 0 ) ]


trenchHelp : List Position -> List Instruction -> List Position
trenchHelp revPositions plan =
    case ( revPositions, plan ) of
        ( lastPos :: _, instruction :: remainder ) ->
            trenchHelp (move lastPos instruction :: revPositions) remainder

        ( _, _ ) ->
            List.reverse revPositions


move : Position -> Instruction -> Position
move ( x, y ) { direction, distance } =
    case direction of
        Up ->
            ( x, y - distance )

        Down ->
            ( x, y + distance )

        Left ->
            ( x - distance, y )

        Right ->
            ( x + distance, y )


area : List Position -> Int
area positions =
    let
        len =
            length positions
    in
    case positions of
        head :: tail ->
            let
                shifted =
                    List.append tail [ head ]
            in
            List.map2 (\pos1 pos2 -> ( pos1, pos2 )) positions shifted
                |> List.foldl (\( ( x1, y1 ), ( x2, y2 ) ) sum -> sum + (x1 * y2 - x2 * y1)) 0
                |> (\sum -> round <| toFloat sum * 0.5)
                |> (\sum -> abs sum + len // 2 + 1)

        _ ->
            0


length : List Position -> Int
length positions =
    case positions of
        head :: tail ->
            let
                shifted =
                    List.append tail [ head ]
            in
            List.map2 (\pos1 pos2 -> ( pos1, pos2 )) positions shifted
                |> List.foldl (\( ( x1, y1 ), ( x2, y2 ) ) sum -> sum + abs (x1 - x2 + y1 - y2)) 0

        _ ->
            0


parser : Parser Plan
parser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item =
            succeed ColoredInstruction
                |= directionParser
                |. spaces
                |= int
                |. spaces
                |= colorInstructionParser
        , trailing = Optional
        }


colorInstructionParser : Parser Instruction
colorInstructionParser =
    succeed (\a b c d e f -> ( a * 16 ^ 4 + b * 16 ^ 3 + c * 16 ^ 2 + d * 16 + e, f ))
        |. symbol "("
        |. symbol "#"
        |= halfByteParser
        |= halfByteParser
        |= halfByteParser
        |= halfByteParser
        |= halfByteParser
        |= halfByteParser
        |. symbol ")"
        |> Parser.andThen toInstruction


toInstruction : ( Int, Int ) -> Parser Instruction
toInstruction ( distance, encodedDirection ) =
    case encodedDirection of
        0 ->
            succeed { distance = distance, direction = Right }

        1 ->
            succeed { distance = distance, direction = Down }

        2 ->
            succeed { distance = distance, direction = Left }

        3 ->
            succeed { distance = distance, direction = Up }

        _ ->
            problem "unknown encoding for direction"


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
