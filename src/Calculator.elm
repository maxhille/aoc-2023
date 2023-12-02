module Calculator exposing (Model, Msg, calculator)

import Browser
import Html exposing (Html, div, h2, p, section, text, textarea)
import Html.Attributes exposing (cols, rows, style)
import Html.Events exposing (onInput)


type alias Model =
    { output : Output
    }


type alias Error =
    String


type Msg
    = OnInput String


type Output
    = NoInput
    | HasResult (Result Error Int)


init : Model
init =
    { output = NoInput
    }


update : (String -> Result Error Int) -> Msg -> Model -> Model
update calculate msg model =
    case msg of
        OnInput input ->
            { model | output = HasResult <| calculate input }


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
                    HasResult result ->
                        case result of
                            Ok int ->
                                text <| String.fromInt int

                            Err error ->
                                text <| "Error: " ++ error

                    NoInput ->
                        text "No input yet"
                ]
            ]
        ]


calculator : (String -> Result error Int) -> (error -> String) -> Program () Model Msg
calculator calculate formatError =
    Browser.sandbox { init = init, update = update (calculate >> Result.mapError formatError), view = view }
