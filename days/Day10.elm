module Day10 exposing
    ( Direction(..)
    , Map
    , Tile(..)
    , calculatePart1
    , calculatePart2
    , getAt
    , parser
    , pipeAtPos
    , puzzle
    , startPosition
    )

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
    , dir : Direction
    }


calculatePart1 : String -> Result Error Int
calculatePart1 =
    Parser.run parser
        >> Result.mapError Parser.deadEndsToString
        >> Result.andThen path
        >> Result.map (\path_ -> List.length path_ // 2)


calculatePart2 : String -> Result Error Int
calculatePart2 =
    Parser.run parser
        >> Result.mapError Parser.deadEndsToString
        >> Result.andThen
            (\map ->
                path map
                    |> Result.andThen (\path_ -> countEnclosedTiles path_ map)
            )


countEnclosedTiles : List Position -> Map -> Result Error Int
countEnclosedTiles path_ map =
    startPosition map
        |> Result.andThen (\start -> Result.map (\pipe -> ( start, pipe )) <| pipeAtPos start map)
        |> Result.map (\( start, ( dir1, dir2 ) ) -> replace start (Pipe ( dir1, dir2 )) map)
        |> Result.map (List.indexedMap (countEnclosedRowTiles path_) >> List.sum)


countEnclosedRowTiles : List Position -> Int -> List Tile -> Int
countEnclosedRowTiles path_ y tiles =
    List.foldl
        (\( x, tile ) acc ->
            let
                cleanedTile =
                    if List.member ( x, y ) path_ then
                        tile

                    else
                        Ground

                state =
                    update acc.state cleanedTile
            in
            { acc
                | state = state
                , count =
                    if state.inside && cleanedTile == Ground then
                        acc.count + 1

                    else
                        acc.count
            }
        )
        { state = { inside = False, grinding = NotGrinding }, count = 0 }
        (List.indexedMap Tuple.pair tiles)
        |> .count


update : State -> Tile -> State
update state tile =
    case tile of
        Pipe ( N, E ) ->
            { state | grinding = FromNorth }

        Pipe ( N, S ) ->
            { state | inside = not state.inside }

        Pipe ( N, W ) ->
            case state.grinding of
                FromNorth ->
                    { state
                        | grinding = NotGrinding
                    }

                FromSouth ->
                    { state
                        | inside = not state.inside
                        , grinding = NotGrinding
                    }

                NotGrinding ->
                    -- should never happen
                    state

        Pipe ( E, S ) ->
            { state | grinding = FromSouth }

        Pipe ( E, W ) ->
            state

        Pipe ( S, W ) ->
            case state.grinding of
                FromNorth ->
                    { state
                        | inside = not state.inside
                        , grinding = NotGrinding
                    }

                FromSouth ->
                    { state
                        | grinding = NotGrinding
                    }

                NotGrinding ->
                    -- should never happen
                    state

        _ ->
            state


type alias State =
    { inside : Bool
    , grinding : Grinding
    }


type Grinding
    = NotGrinding
    | FromSouth
    | FromNorth


path : Map -> Result Error (List Position)
path map =
    startPosition map
        |> Result.andThen
            (\start ->
                pipeAtPos start map
                    |> Result.andThen
                        (\( dir1, dir2 ) ->
                            pathHelp start [] { pos = start, dir = dir1 } (replace start (Pipe ( dir1, dir2 )) map)
                        )
            )


pathHelp : Position -> List Position -> Ghost -> Map -> Result Error (List Position)
pathHelp start positions ghost map =
    if positions /= [] && ghost.pos == start then
        Ok <| positions

    else
        let
            ghostResult =
                advance ghost map
        in
        case ghostResult of
            Ok ghostNext ->
                pathHelp start (ghostNext.pos :: positions) ghostNext map

            Err error ->
                Err error


replace : Position -> Tile -> Map -> Map
replace ( x, y ) tile map =
    List.concat
        [ List.take y map
        , [ case List.drop y map of
                row :: _ ->
                    List.concat
                        [ List.take x row
                        , case List.drop x row of
                            _ :: _ ->
                                [ tile ]

                            _ ->
                                []
                        , List.drop (x + 1) row
                        ]

                _ ->
                    []
          ]
        , List.drop (y + 1) map
        ]


pipeAtPos : Position -> Map -> Result Error ( Direction, Direction )
pipeAtPos pos map =
    [ N, E, S, W ]
        |> List.filterMap
            (\dir ->
                let
                    neighbourPos =
                        move pos dir

                    neighbourTile =
                        getAt neighbourPos map

                    pointsBack ( dir1, dir2 ) =
                        opposite dir1 == dir || opposite dir2 == dir
                in
                case neighbourTile of
                    Pipe pipeDirs ->
                        if pointsBack pipeDirs then
                            Just dir

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> (\exits ->
                case exits of
                    [ exit1, exit2 ] ->
                        Ok ( exit1, exit2 )

                    _ ->
                        Err "position is no path pipe: has wrong number of exits"
           )


advance : Ghost -> Map -> Result Error Ghost
advance ghost map =
    let
        nextPos =
            move ghost.pos ghost.dir

        nextTile =
            getAt nextPos map
    in
    case nextTile of
        Pipe ( p1, p2 ) ->
            if p1 == opposite ghost.dir then
                Ok { pos = nextPos, dir = p2 }

            else if p2 == opposite ghost.dir then
                Ok { pos = nextPos, dir = p1 }

            else
                Err "ghost ran into a pipe wall"

        _ ->
            Err "ghost tried to exit the pipe"


opposite : Direction -> Direction
opposite direction =
    case direction of
        N ->
            S

        E ->
            W

        S ->
            N

        W ->
            E


startPosition : Map -> Result Error Position
startPosition =
    Result.fromMaybe "could not find start tile"
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
    , calculatePart2 = calculatePart2
    }
