port module Ui exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation exposing (Key, load, pushUrl)
import Html exposing (Html, a, button, h1, h2, h3, li, main_, nav, p, section, text, textarea, ul)
import Html.Attributes exposing (href, spellcheck, value)
import Html.Events exposing (onClick, onInput)
import Puzzle exposing (Puzzle)
import Shared exposing (Request, Response, toResult)
import Url exposing (Url)


port fromWorker : (Response -> msg) -> Sub msg


port toWorker : Request -> Cmd msg


type alias Model =
    { input : String
    , key : Key
    , day : Int
    , puzzles : List Puzzle
    , calculation1 : Calculation
    , calculation2 : Calculation
    }


type Msg
    = OnInput String
    | OnUrlChange Url
    | OnUrlRequest UrlRequest
    | CalculatePart Int
    | OnPartResult Int (Result String Int)


type Calculation
    = NotStarted
    | Problem String
    | Progress
    | Finished Int


fromUrl : Url -> Int
fromUrl =
    .fragment
        >> Maybe.andThen String.toInt
        >> Maybe.withDefault 1


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { input = ""
      , key = key
      , day = fromUrl url
      , puzzles = Shared.puzzles
      , calculation1 = NotStarted
      , calculation2 = NotStarted
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnInput input ->
            ( { model
                | input = input
                , calculation1 = NotStarted
                , calculation2 = NotStarted
              }
            , Cmd.none
            )

        OnUrlChange url ->
            ( { model
                | day = fromUrl url
                , input = ""
              }
            , Cmd.none
            )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , load url
                    )

        CalculatePart part ->
            ( case part of
                1 ->
                    { model | calculation1 = Progress }

                2 ->
                    { model | calculation2 = Progress }

                _ ->
                    model
            , toWorker { day = model.day, part = part, input = model.input }
            )

        OnPartResult part result ->
            if part == 1 then
                ( { model
                    | calculation1 =
                        case result of
                            Ok int ->
                                Finished int

                            Err error ->
                                Problem error
                  }
                , Cmd.none
                )

            else
                ( { model
                    | calculation2 =
                        case result of
                            Ok int ->
                                Finished int

                            Err error ->
                                Problem error
                  }
                , Cmd.none
                )


view : Model -> Browser.Document Msg
view model =
    let
        selectedPuzzle =
            model.puzzles
                |> List.drop (model.day - 1)
                |> List.head

        name day =
            "Day " ++ String.fromInt day
    in
    { title = "AoC 2023"
    , body =
        [ h1 [] [ text "Advent of Code 2023" ]
        , main_ []
            [ nav []
                [ h2 [] [ text "Puzzles" ]
                , ul [] <|
                    List.indexedMap
                        (\i _ ->
                            li []
                                [ let
                                    day =
                                        i + 1
                                  in
                                  a [ href <| "#" ++ String.fromInt day ] [ text <| name day ]
                                ]
                        )
                        model.puzzles
                ]
            , section [] <|
                [ h2 [] [ text <| name model.day ]
                , let
                    link =
                        "https://adventofcode.com/2023/day/" ++ String.fromInt model.day
                  in
                  a [ href link ] [ text link ]
                ]
                    ++ (case selectedPuzzle of
                            Just puzzle ->
                                [ section []
                                    [ h3 [] [ text "Input" ]
                                    , textarea [ onInput OnInput, spellcheck False, value model.input ] []
                                    ]
                                , section []
                                    [ h3 [] [ text "Output" ]
                                    , viewCalculation model.calculation1 puzzle model.input 1 CalculatePart
                                    , viewCalculation model.calculation2 puzzle model.input 2 CalculatePart
                                    ]
                                ]

                            Nothing ->
                                [ p [] [ text <| "there is no puzzle implementation for Day " ++ String.fromInt model.day ]
                                ]
                       )
            ]
        ]
    }


viewCalculation : Calculation -> Puzzle -> String -> Int -> (Int -> Msg) -> Html Msg
viewCalculation calculation puzzle input part msg =
    p []
        [ case calculation of
            NotStarted ->
                case puzzle.validate input of
                    Ok _ ->
                        button [ onClick <| msg part ] [ text <| "Calculate Part " ++ String.fromInt part ]

                    Err error ->
                        text <| "Input invalid: " ++ error

            Problem str ->
                text <| "There was a problem with the calculation: " ++ str

            Finished int ->
                text <| "Result Part " ++ String.fromInt part ++ ": " ++ String.fromInt int

            Progress ->
                text <| "Working..."
        ]


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> fromWorker (toResult >> (\( part, result ) -> OnPartResult part result))
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }
