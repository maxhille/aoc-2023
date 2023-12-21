module Shared exposing (Request, Response, puzzles, toResponse, toResult)

import Day1
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Puzzle exposing (Puzzle)


puzzles : List Puzzle
puzzles =
    [ Day1.puzzle
    , Day2.puzzle
    , Day3.puzzle
    , Day4.puzzle
    , Day5.puzzle
    , Day6.puzzle
    , Day7.puzzle
    , Day8.puzzle
    , Day9.puzzle
    , Day10.puzzle
    , Day11.puzzle
    , Day12.puzzle
    , Day13.puzzle
    , Day14.puzzle
    , Day15.puzzle
    ]


type alias Request =
    { day : Int
    , part : Int
    , input : String
    }


type alias Response =
    { success : Bool
    , value : Maybe Int
    , error : Maybe String
    , part : Int
    }


toResult : Response -> ( Int, Result String Int )
toResult response =
    if response.success then
        ( response.part, Ok <| Maybe.withDefault -1 <| response.value )

    else
        ( response.part, Err <| Maybe.withDefault "(no error message in response)" <| response.error )


toResponse : Int -> Result String Int -> Response
toResponse part result =
    case result of
        Ok value ->
            { part = part, success = True, value = Just value, error = Nothing }

        Err error ->
            { part = part, success = False, value = Nothing, error = Just error }
