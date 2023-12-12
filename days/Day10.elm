module Day10 exposing (Direction(..), Map, Tile(..), calculatePart1, getAt, getStart, parser, puzzle)

import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), andThen, loop, map, oneOf, problem, succeed, symbol)
import Puzzle exposing (Puzzle)


type alias Map =
    List (List Tile)


type alias Position =
    ( Int, Int )


type Tile
    = Pipe ( Direction, Direction )
    | Start
    | Ground


type Direction
    = N
    | E
    | S
    | W


type alias Error =
    String


type alias Ghost =
    { pos : Position
    , entryDir : Direction
    }


calculatePart1 : String -> Result Error Int
calculatePart1 input =
    Parser.run parser input
        |> Result.mapError Parser.deadEndsToString
        |> Result.andThen pathLength


pathLength : Map -> Result Error Int
pathLength map =
    getStart map
        |> Result.andThen (\start -> spawn start map)
        |> Result.andThen (\( ghost1, ghost2 ) -> pathLengthHelp 1 ghost1 ghost2 map)


pathLengthHelp : Int -> Ghost -> Ghost -> Map -> Result Error Int
pathLengthHelp steps ghost1 ghost2 map =
    if ghost1.pos == ghost2.pos then
        Ok steps

    else
        let
            ghostResult1 =
                advance ghost1 map

            ghostResult2 =
                advance ghost2 map
        in
        case ( ghostResult1, ghostResult2 ) of
            ( Ok ghostNext1, Ok ghostNext2 ) ->
                pathLengthHelp (steps + 1) ghostNext1 ghostNext2 map

            ( _, _ ) ->
                Err "Ghosts could not advance"


spawn : Position -> Map -> Result Error ( Ghost, Ghost )
spawn pos map =
    [ N, E, S, W ]
        |> List.filterMap
            (\dir ->
                let
                    neighbourPos =
                        move pos dir

                    neighbourTile =
                        getAt neighbourPos map

                    neighbourEntryDir =
                        invertDirection dir
                in
                case neighbourTile of
                    Pipe ( dirP1, dirP2 ) ->
                        if neighbourEntryDir == dirP1 || neighbourEntryDir == dirP2 then
                            Just { pos = neighbourPos, entryDir = invertDirection dir }

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> (\ghosts ->
                case ghosts of
                    [ ghost1, ghost2 ] ->
                        Ok ( ghost1, ghost2 )

                    _ ->
                        Err "wrong number of ghosts"
           )


advance : Ghost -> Map -> Result Error Ghost
advance ghost map =
    let
        pipe =
            getAt ghost.pos map
    in
    case pipe of
        Pipe ( p1, p2 ) ->
            let
                exitDirection =
                    if ghost.entryDir == p1 then
                        p2

                    else
                        p1

                nextPosition =
                    move ghost.pos exitDirection

                nextEntryDirection =
                    invertDirection exitDirection
            in
            Ok { pos = nextPosition, entryDir = nextEntryDirection }

        _ ->
            Err "clipped through pipe wall"


invertDirection : Direction -> Direction
invertDirection direction =
    case direction of
        N ->
            S

        E ->
            W

        S ->
            N

        W ->
            E


getStart : Map -> Result Error Position
getStart =
    Result.fromMaybe "could not find start"
        << List.foldl
            (\( cury, row ) xy ->
                case xy of
                    Just ( x, y_ ) ->
                        Just ( x, y_ )

                    Nothing ->
                        let
                            xStart =
                                List.foldl
                                    (\( curx, tile ) x ->
                                        case x of
                                            Just x_ ->
                                                Just x_

                                            Nothing ->
                                                if tile == Start then
                                                    Just curx

                                                else
                                                    Nothing
                                    )
                                    Nothing
                                    (List.indexedMap Tuple.pair row)
                        in
                        xStart |> Maybe.map (\xStart_ -> ( xStart_, cury ))
            )
            Nothing
        << List.indexedMap Tuple.pair


move : Position -> Direction -> Position
move ( x, y ) direction =
    let
        ( dx, dy ) =
            moveStep direction
    in
    ( x + dx, y + dy )


moveStep : Direction -> ( Int, Int )
moveStep direction =
    case direction of
        N ->
            ( 0, -1 )

        E ->
            ( 1, 0 )

        S ->
            ( 0, 1 )

        W ->
            ( -1, 0 )


getAt : Position -> Map -> Tile
getAt ( x, y ) =
    List.drop y
        >> List.head
        >> Maybe.withDefault []
        >> List.drop x
        >> List.head
        >> Maybe.withDefault Ground


parser : Parser Map
parser =
    loop [] parserHelp


parserHelp : Map -> Parser (Step Map Map)
parserHelp revRows =
    oneOf
        [ succeed (\row -> Loop (row :: revRows))
            |= rowParser
        , succeed (Loop revRows)
            |. symbol "\n"
        , succeed (Loop revRows)
            |. symbol " "
        , succeed ()
            |> map (\_ -> Done (List.reverse revRows))
        ]


rowParser : Parser (List Tile)
rowParser =
    loop [] rowParserHelp


rowParserHelp : List Tile -> Parser (Step (List Tile) (List Tile))
rowParserHelp revTiles =
    oneOf
        [ succeed (\tile -> Loop (tile :: revTiles))
            |= tileParser
            |. Parser.chompWhile (\c -> c == ' ')
        , succeed ()
            |> andThen
                (\_ ->
                    case revTiles of
                        [] ->
                            problem "no ints here"

                        _ ->
                            succeed ()
                )
            |> map (\_ -> Done <| List.reverse revTiles)
        ]


tileParser : Parser Tile
tileParser =
    oneOf
        [ succeed (Pipe ( N, S ))
            |. symbol "|"
        , succeed (Pipe ( E, W ))
            |. symbol "-"
        , succeed (Pipe ( N, E ))
            |. symbol "L"
        , succeed (Pipe ( N, W ))
            |. symbol "J"
        , succeed (Pipe ( S, W ))
            |. symbol "7"
        , succeed (Pipe ( E, S ))
            |. symbol "F"
        , succeed Ground
            |. symbol "."
        , succeed Start
            |. symbol "S"
        ]


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = calculatePart1
    , calculatePart2 = (\_ -> Ok 1) >> Result.mapError Parser.deadEndsToString
    }
