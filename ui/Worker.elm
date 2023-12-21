port module Worker exposing (main)

import Platform
import Shared exposing (Request, Response, puzzles, toResponse)


port fromUi : (Request -> msg) -> Sub msg


port toUi : Response -> Cmd msg


run : Request -> Response
run { part, day, input } =
    puzzles
        |> List.drop (day - 1)
        |> List.head
        |> (\maybePuzzle ->
                case maybePuzzle of
                    Just puzzle ->
                        case part of
                            1 ->
                                puzzle.calculatePart1 input

                            2 ->
                                puzzle.calculatePart2 input

                            _ ->
                                Err "part was not 1 or 2"

                    Nothing ->
                        Err "worker could not find puzzle"
           )
        |> toResponse part


main : Program () () Request
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \request _ -> ( (), toUi <| run request )
        , subscriptions = \_ -> fromUi identity
        }
