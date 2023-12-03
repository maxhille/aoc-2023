module Ui exposing (Ui, ui)

import Browser
import Html exposing (Html, div, h2, section, text, textarea)
import Html.Attributes exposing (cols, rows, style)
import Html.Events exposing (onInput)


type alias Model =
    { input : String
    }


type Msg
    = OnInput String


init : Model
init =
    { input = ""
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnInput input ->
            { model | input = input }


view : List Output -> Model -> Html Msg
view outputs model =
    div [ style "padding" "5em" ]
        [ section []
            [ h2 [] [ text "Input" ]
            , textarea [ onInput OnInput, cols 120, rows 30 ] []
            ]
        , outputs
            |> List.map
                (\output ->
                    div []
                        [ h2 [] [ text <| "Output " ++ output.title ]
                        , output.view model.input
                        ]
                )
            |> section []
        ]


type alias Ui =
    Program () Model Msg


type alias Output =
    { title : String
    , view : String -> Html Msg
    }


ui : List Output -> Ui
ui outputs =
    Browser.sandbox { init = init, update = update, view = view outputs }
