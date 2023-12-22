module Day17 exposing (calculatePart1, calculatePart2, parser, puzzle)

import Dict exposing (Dict)
import List.Extra
import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..))
import Puzzle exposing (Puzzle)
import Set exposing (Set)


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Position =
    ( Int, Int )


type alias Error =
    String


calculatePart1 : Grid -> Result Error Int
calculatePart1 grid =
    let
        width =
            grid |> List.head |> Maybe.map List.length |> Maybe.withDefault 0

        height =
            grid |> List.length

        raGrid =
            List.indexedMap
                (\y row ->
                    List.indexedMap
                        (\x heat ->
                            ( ( x, y ), heat )
                        )
                        row
                )
                grid
                |> List.concat
                |> Dict.fromList
    in
    astar ( 0, 0 ) ( width - 1, height - 1 ) (crucibleMoves raGrid) |> Result.fromMaybe "Dijkstra unsuccessful"


calculatePart2 : Grid -> Result Error Int
calculatePart2 grid =
    let
        w =
            grid |> List.head |> Maybe.map List.length |> Maybe.withDefault 0

        h =
            grid |> List.length

        raGrid =
            List.indexedMap
                (\y row ->
                    List.indexedMap
                        (\x heat ->
                            ( ( x, y ), heat )
                        )
                        row
                )
                grid
                |> List.concat
                |> Dict.fromList
    in
    astar ( 0, 0 ) ( w - 1, h - 1 ) (ultraCrucibleMoves raGrid) |> Result.fromMaybe "A* Star unsuccessful"


type alias Node =
    { position : Position
    , lastDir : Direction
    , lastDirCount : Int
    }


type alias Grid =
    List (List Int)


type alias RaGrid =
    Dict Position Int


astar : Position -> Position -> (Node -> List ( Int, Node )) -> Maybe Int
astar start goal getNeighbours =
    astarHelp goal getNeighbours (frontierOfSingleElement ( 0, { position = start, lastDir = Down, lastDirCount = 0 } )) Set.empty


astarHelp : Position -> (Node -> List ( Int, Node )) -> Frontier -> Set NodeKey -> Maybe Int
astarHelp goal getNeighbours frontier expanded =
    case frontierHead goal frontier of
        Nothing ->
            Nothing

        Just ( dist, node ) ->
            if node.position == goal then
                Just dist

            else
                let
                    newExpanded =
                        Set.insert (key node) expanded

                    newFrontier =
                        List.foldl
                            (\( edgeWeight, neighbour ) frontier_ ->
                                if not (Set.member (key neighbour) expanded) && not (frontierMember neighbour frontier_) then
                                    frontierAdd (dist + edgeWeight) neighbour frontier_

                                else
                                    frontierLowerUpdate (dist + edgeWeight) neighbour frontier_
                            )
                            (frontierRemove node frontier)
                            (getNeighbours node)
                in
                astarHelp goal getNeighbours newFrontier newExpanded


type alias NodeKey =
    ( Int, Int, Position )


key : Node -> NodeKey
key node =
    ( case node.lastDir of
        Up ->
            1

        Down ->
            2

        Left ->
            3

        Right ->
            4
    , node.lastDirCount
    , node.position
    )


type alias Frontier =
    Dict NodeKey ( Int, Node )


frontierOfSingleElement : ( Int, Node ) -> Frontier
frontierOfSingleElement ( dist, node ) =
    Dict.singleton (key node) ( dist, node )


frontierHead : Position -> Frontier -> Maybe ( Int, Node )
frontierHead goal frontier =
    List.Extra.minimumBy (\( dist, node ) -> manhattanDist goal node.position + dist) (Dict.values frontier)


frontierMember : Node -> Frontier -> Bool
frontierMember node =
    Dict.member (key node)


frontierAdd : Int -> Node -> Frontier -> Frontier
frontierAdd dist node =
    Dict.insert (key node) ( dist, node )


frontierLowerUpdate : Int -> Node -> Frontier -> Frontier
frontierLowerUpdate newDist node =
    Dict.update (key node)
        (Maybe.map
            (\( oldDist, _ ) ->
                if newDist < oldDist then
                    ( newDist, node )

                else
                    ( oldDist, node )
            )
        )


frontierRemove : Node -> Frontier -> Frontier
frontierRemove node =
    Dict.remove (key node)


crucibleMoves : RaGrid -> Node -> List ( Int, Node )
crucibleMoves grid node =
    List.filterMap
        (\nextDir ->
            if nextDir == oppositeDir node.lastDir then
                Nothing
                -- no point in exploring that path

            else if nextDir == node.lastDir && node.lastDirCount == 3 then
                Nothing
                -- Forbidden

            else
                move1 grid node nextDir
        )
        [ Up, Down, Left, Right ]


move1 : RaGrid -> Node -> Direction -> Maybe ( Int, Node )
move1 grid node nextDir =
    let
        ( x, y ) =
            node.position

        nextPos =
            case nextDir of
                Up ->
                    ( x, y - 1 )

                Down ->
                    ( x, y + 1 )

                Left ->
                    ( x - 1, y )

                Right ->
                    ( x + 1, y )

        nextBlock =
            Dict.get nextPos grid

        sameDirCount =
            if nextDir == node.lastDir then
                node.lastDirCount

            else
                0
    in
    nextBlock
        |> Maybe.map
            (\nextWeight ->
                ( nextWeight, { position = nextPos, lastDir = nextDir, lastDirCount = 1 + sameDirCount } )
            )


ultraCrucibleMoves : RaGrid -> Node -> List ( Int, Node )
ultraCrucibleMoves grid node =
    List.filterMap
        (\nextDir ->
            if nextDir == oppositeDir node.lastDir then
                Nothing

            else if nextDir == node.lastDir && node.lastDirCount == 10 then
                Nothing

            else if nextDir == node.lastDir && node.lastDirCount /= 0 then
                move1 grid node nextDir

            else
                move1 grid node nextDir
                    |> Maybe.andThen (\( w1, n1 ) -> move1 grid n1 nextDir |> Maybe.map (\( w2, n2 ) -> ( w1 + w2, n2 )))
                    |> Maybe.andThen (\( s2, n2 ) -> move1 grid n2 nextDir |> Maybe.map (\( w3, n3 ) -> ( s2 + w3, n3 )))
                    |> Maybe.andThen (\( s3, n3 ) -> move1 grid n3 nextDir |> Maybe.map (\( w4, n4 ) -> ( s3 + w4, n4 )))
        )
        [ Up, Down, Left, Right ]


oppositeDir : Direction -> Direction
oppositeDir dir =
    case dir of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


manhattanDist : Position -> Position -> Int
manhattanDist ( xLeft, yLeft ) ( xRight, yRight ) =
    abs (xLeft - xRight) + abs (yLeft - yRight)


parser : Parser Grid
parser =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = ""
        , spaces = Parser.chompWhile (always False)
        , item = rowParser
        , trailing = Optional
        }


rowParser : Parser (List Int)
rowParser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = Parser.chompWhile (always False)
        , item = digitParser
        , trailing = Optional
        }


digitParser : Parser Int
digitParser =
    Parser.getChompedString (Parser.chompIf Char.isDigit)
        |> Parser.andThen (\s -> Parser.succeed (String.toInt s |> Maybe.withDefault 12345))


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
