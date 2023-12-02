module Calculator exposing (Error(..), Model, Msg, calculator)

import Browser
import Html exposing (Html, div, h2, li, p, section, text, textarea, ul)
import Html.Attributes exposing (cols, rows, style)
import Html.Events exposing (onInput)


type Error
    = NoInput
    | BadLines (List String)


type alias Model =
    { output : Result Error Int
    }


type Msg
    = OnInput String


init : Model
init =
    { output = Err NoInput
    }


update : (String -> Result Error Int) -> Msg -> Model -> Model
update calculate msg model =
    case msg of
        OnInput input ->
            { model | output = calculate input }


view : Model -> Html Msg
view model =
    div [ style "padding" "5em" ]
        [ section []
            [ h2 [] [ text "Input" ]
            , textarea [ onInput OnInput, cols 120, rows 30 ] []
            ]
        , section []
            [ h2 [] [ text "Output" ]
            , p []
                [ case model.output of
                    Ok output ->
                        output |> String.fromInt |> text

                    Err error ->
                        case error of
                            NoInput ->
                                text "No input yet"

                            BadLines lines ->
                                ul [] <| List.map (\line -> li [] [ text <| "Bad line: " ++ line ]) lines
                ]
            ]
        ]


calculator : (String -> Result Error Int) -> Program () Model Msg
calculator calculate =
    Browser.sandbox { init = init, update = update calculate, view = view }
