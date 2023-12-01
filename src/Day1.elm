module Day1 exposing (Msg(..), calculate, main, update, view)

import Browser
import Html exposing (Html, div, h2, li, p, section, text, textarea, ul)
import Html.Attributes exposing (cols, rows, style)
import Html.Events exposing (onInput)


calculate : String -> Result Error Int
calculate input =
    let
        lines : List String
        lines =
            input
                |> String.trim
                |> String.lines
                |> List.map String.trim

        digits : String -> List String
        digits line =
            String.filter Char.isDigit line
                |> String.toList
                |> List.map String.fromChar

        combine : String -> Line
        combine line =
            case digits line |> (\ns -> ( List.head ns, List.head <| List.reverse ns )) of
                ( Just first, Just last ) ->
                    case String.toInt <| first ++ last of
                        Just value ->
                            GoodLine value

                        Nothing ->
                            BadLine line

                ( _, _ ) ->
                    BadLine line
    in
    lines
        |> List.map combine
        |> List.foldl
            (\line acc ->
                case line of
                    BadLine badLine ->
                        { acc | errors = badLine :: acc.errors }

                    GoodLine value ->
                        { acc | value = acc.value + value }
            )
            { value = 0
            , errors = []
            }
        |> (\{ value, errors } ->
                if errors == [] then
                    Ok value

                else
                    Err <| BadLines errors
           )


type Line
    = BadLine String
    | GoodLine Int


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


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnInput input ->
            { model | output = calculate input }


view : Model -> Html Msg
view model =
    div [ style "padding" "5em" ]
        [ section []
            [ h2 [] [ text "Input" ]
            , textarea [ onInput OnInput, cols 40, rows 10 ] []
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


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
