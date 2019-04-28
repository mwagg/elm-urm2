module Main exposing (main)

import Browser
import Html exposing (Html, text)



--- MODEL ---


type alias Model =
    ()


init : Model
init =
    ()



--- UPDATE ---


type alias Msg =
    ()


update : Msg -> Model -> Model
update msg model =
    model



--- VIEW ---


view : Model -> Html Msg
view model =
    text "Hello world"



--- MAIN ---


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
