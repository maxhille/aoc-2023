module Day17 exposing (calculatePart1, calculatePart2, parser, puzzle)

import Dict exposing (Dict)
import List.Extra
import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), oneOf, symbol)
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
    dijkstra ( 0, 0 ) ( w - 1, h - 1 ) raGrid |> Result.fromMaybe "Dijkstra unsuccessful"


calculatePart2 : Grid -> Result Error Int
calculatePart2 grid =
    Err "not implemented"


type alias Node =
    { position : Position
    , lastDir : Direction
    , lastDirCount : Int
    }


type alias Grid =
    List (List Int)


type alias RaGrid =
    Dict Position Int


dijkstra : Position -> Position -> RaGrid -> Maybe Int
dijkstra start goal grid =
    dijkstraHelp goal grid (frontierOfSingleElement ( 0, { position = start, lastDir = Down, lastDirCount = 0 } )) Set.empty


dijkstraHelp : Position -> RaGrid -> Frontier -> Set NodeKey -> Maybe Int
dijkstraHelp goal grid frontier expanded =
    case frontierHead goal frontier of
        Nothing ->
            Nothing

        Just ( dist, node ) ->
            if node.position == goal then
                Just dist

            else
                let
                    {- _ =
                           Debug.log "next node" node

                       _ =
                           Debug.log "frontier size" (Dict.size frontier)
                    -}
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
                            (dijkstraNeighbours node grid)
                in
                dijkstraHelp goal grid newFrontier newExpanded


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


dijkstraNeighbours : Node -> RaGrid -> List ( Int, Node )
dijkstraNeighbours node grid =
    List.filterMap
        (\nextDir ->
            if nextDir == oppositeDir node.lastDir then
                Nothing
                -- no point in exploring that path

            else if nextDir == node.lastDir && node.lastDirCount == 3 then
                Nothing
                -- Forbidden

            else
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
        )
        [ Up, Down, Left, Right ]


weightAt : Position -> Grid -> Maybe Int
weightAt ( x, y ) grid =
    if x < 0 || y < 0 then
        Nothing

    else
        grid
            |> List.drop y
            |> List.head
            |> Maybe.andThen (List.drop x >> List.head)


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
