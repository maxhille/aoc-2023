module Day16 exposing (Field(..), calculatePart1, calculatePart2, parser, puzzle)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), oneOf, symbol)
import Puzzle exposing (Puzzle)
import Set exposing (Set)


type alias Contraption =
    List (List Field)


type Field
    = Empty
    | Vertical
    | Horizontal
    | Slash
    | Backslash


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Position =
    ( Int, Int )


type alias Beam =
    { position : Position, direction : Direction }


type Interaction
    = Split Direction Direction
    | Single Direction


type alias Error =
    String


calculatePart1 : Contraption -> Result Error Int
calculatePart1 contraption =
    energize contraption [ { position = ( 0, 0 ), direction = Right } ] |> Ok


calculatePart2 : Contraption -> Result Error Int
calculatePart2 contraption =
    let
        width =
            contraption |> List.head |> Maybe.map List.length |> Maybe.withDefault 0

        height =
            contraption |> List.length
    in
    List.concat
        [ List.range 0 (width - 1) |> List.map (\x -> { position = ( x, 0 ), direction = Down })
        , List.range 0 (width - 1) |> List.map (\x -> { position = ( x, height - 1 ), direction = Up })
        , List.range 0 (height - 1) |> List.map (\y -> { position = ( 0, y ), direction = Right })
        , List.range 0 (height - 1) |> List.map (\y -> { position = ( width - 1, y ), direction = Left })
        ]
        |> energize contraption
        |> Ok


energize : Contraption -> List Beam -> Int
energize contraption =
    let
        precomputed =
            precomputeSplitters contraption
    in
    List.map
        (energizeHelp precomputed Set.empty contraption)
        >> List.map Set.size
        >> List.maximum
        >> Maybe.withDefault 0


energizeHelp : Dict Key (Set Position) -> Set Position -> Contraption -> Beam -> Set Position
energizeHelp dict positions contraption beam =
    case advance contraption beam of
        -- beam hit splitter
        _ :: _ :: _ ->
            case Dict.get (key beam) dict of
                Just precomputed ->
                    Set.union precomputed positions

                Nothing ->
                    Set.empty

        -- beam exited contraption
        [] ->
            positions

        [ updatedBeam ] ->
            energizeHelp dict (Set.insert beam.position positions) contraption updatedBeam


precomputeSplitters : Contraption -> Dict Key (Set Position)
precomputeSplitters contraption =
    splitterBeams contraption
        |> List.foldl
            (\{ beam1, beam2 } dict ->
                let
                    energized =
                        precomputeSplittersHelp Dict.empty [ beam1, beam2 ] contraption
                            |> Dict.keys
                            |> List.map Tuple.first
                            |> Set.fromList
                in
                dict
                    |> Dict.insert (key beam1) energized
                    |> Dict.insert (key beam2) energized
            )
            Dict.empty


precomputeSplittersHelp : Dict Key () -> List Beam -> Contraption -> Dict Key ()
precomputeSplittersHelp dict beams contraption =
    if beams == [] then
        dict

    else
        let
            updatedDict =
                beams
                    |> List.map (\beam -> ( key beam, () ))
                    |> Dict.fromList
                    |> Dict.union dict

            updatedBeams =
                beams
                    |> List.map (advance contraption)
                    |> List.concat
                    |> List.filter (\beam -> not <| Dict.member (key beam) dict)
        in
        precomputeSplittersHelp updatedDict updatedBeams contraption


splitterBeams : Contraption -> List { beam1 : Beam, beam2 : Beam }
splitterBeams =
    indexedMap
        (\position field ->
            case field of
                Horizontal ->
                    Just
                        { beam1 = { position = position, direction = Up }
                        , beam2 = { position = position, direction = Down }
                        }

                Vertical ->
                    Just
                        { beam1 = { position = position, direction = Left }
                        , beam2 = { position = position, direction = Right }
                        }

                _ ->
                    Nothing
        )
        >> List.filterMap identity


indexedMap : (Position -> Field -> a) -> Contraption -> List a
indexedMap fn =
    List.indexedMap Tuple.pair
        >> List.map (\( y, row ) -> row |> List.indexedMap Tuple.pair |> List.map (\( x, field ) -> fn ( x, y ) field))
        >> List.concat


type alias Key =
    ( Position, Int )


key : Beam -> Key
key beam =
    ( beam.position
    , case beam.direction of
        Up ->
            0

        Down ->
            1

        Left ->
            2

        Right ->
            3
    )


advance : Contraption -> Beam -> List Beam
advance contraption beam =
    fieldAt beam contraption
        |> Maybe.map
            (\field ->
                case interact field beam of
                    Split direction1 direction2 ->
                        [ move beam direction1, move beam direction2 ]

                    Single direction ->
                        [ move beam direction ]
            )
        |> Maybe.withDefault []
        |> List.filter (\beam_ -> fieldAt beam_ contraption /= Nothing)


move : Beam -> Direction -> Beam
move beam direction =
    let
        ( x, y ) =
            beam.position
    in
    { position =
        case direction of
            Up ->
                ( x, y - 1 )

            Down ->
                ( x, y + 1 )

            Left ->
                ( x - 1, y )

            Right ->
                ( x + 1, y )
    , direction = direction
    }


interact : Field -> Beam -> Interaction
interact field beam =
    case ( field, beam.direction ) of
        ( Vertical, Right ) ->
            Split Up Down

        ( Vertical, Left ) ->
            Split Up Down

        ( Horizontal, Up ) ->
            Split Left Right

        ( Horizontal, Down ) ->
            Split Left Right

        ( Slash, Up ) ->
            Single Right

        ( Slash, Down ) ->
            Single Left

        ( Slash, Left ) ->
            Single Down

        ( Slash, Right ) ->
            Single Up

        ( Backslash, Up ) ->
            Single Left

        ( Backslash, Down ) ->
            Single Right

        ( Backslash, Left ) ->
            Single Up

        ( Backslash, Right ) ->
            Single Down

        _ ->
            Single beam.direction


fieldAt : Beam -> Contraption -> Maybe Field
fieldAt beam contraption =
    let
        ( x, y ) =
            beam.position
    in
    if x < 0 || y < 0 then
        Nothing

    else
        contraption
            |> List.drop y
            |> List.head
            |> Maybe.andThen (List.drop x >> List.head)


parser : Parser Contraption
parser =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = ""
        , spaces = Parser.chompWhile (always False)
        , item = rowParser
        , trailing = Optional
        }


rowParser : Parser (List Field)
rowParser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = Parser.chompWhile (always False)
        , item =
            oneOf
                [ Parser.succeed Backslash
                    |. symbol "\\"
                , Parser.succeed Slash
                    |. symbol "/"
                , Parser.succeed Empty
                    |. symbol "."
                , Parser.succeed Horizontal
                    |. symbol "-"
                , Parser.succeed Vertical
                    |. symbol "|"
                ]
        , trailing = Optional
        }


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
