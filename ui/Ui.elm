module Ui exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation exposing (Key, load, pushUrl)
import Day1
import Day10
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Html exposing (Html, a, button, h1, h2, h3, li, main_, nav, p, section, text, textarea, ul)
import Html.Attributes exposing (href, spellcheck, value)
import Html.Events exposing (onClick, onInput)
import Puzzle exposing (Puzzle)
import Url exposing (Url)


type alias Model =
    { input : String
    , key : Key
    , day : Int
    , puzzles : List (Maybe Puzzle)
    , calculation1 : Calculation
    , calculation2 : Calculation
    }


type Msg
    = OnInput String
    | OnUrlChange Url
    | OnUrlRequest UrlRequest
    | CalculatePart Int Puzzle


type Calculation
    = NotStarted
    | Problem String
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
      , puzzles =
            [ Just Day1.puzzle
            , Just Day2.puzzle
            , Just Day3.puzzle
            , Just Day4.puzzle
            , Just Day5.puzzle
            , Just Day6.puzzle
            , Just Day7.puzzle
            , Just Day8.puzzle
            , Just Day9.puzzle
            , Just Day10.puzzle
            , Nothing
            , Nothing
            , Nothing
            , Nothing
            , Nothing
            , Nothing
            , Nothing
            , Nothing
            , Nothing
            ]
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

        CalculatePart part puzzle ->
            if part == 1 then
                ( { model
                    | calculation1 =
                        case puzzle.calculatePart1 model.input of
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
                        case puzzle.calculatePart2 model.input of
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
                |> Maybe.withDefault Nothing

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
                        (\i puzzle ->
                            li []
                                [ let
                                    day =
                                        i + 1
                                  in
                                  case puzzle of
                                    Just _ ->
                                        a [ href <| "#" ++ String.fromInt day ] [ text <| name day ]

                                    Nothing ->
                                        text <| name day
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


viewCalculation : Calculation -> Puzzle -> String -> Int -> (Int -> Puzzle -> Msg) -> Html Msg
viewCalculation calculation puzzle input part msg =
    p []
        [ case calculation of
            NotStarted ->
                case puzzle.validate input of
                    Ok _ ->
                        button [ onClick <| msg part puzzle ] [ text <| "Calculate Part " ++ String.fromInt part ]

                    Err error ->
                        text <| "Input invalid: " ++ error

            Problem str ->
                text <| "There was a problem with the calculation: " ++ str

            Finished int ->
                text <| "Result Part " ++ String.fromInt part ++ ": " ++ String.fromInt int
        ]


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }
