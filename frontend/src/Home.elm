module Home exposing (Model, init, Msg, update, view)

import Html exposing (div, text, Html)


type alias Model =
    ()


init : Model
init =
    ()


type Msg
    = Show


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Show ->
            ( model, Cmd.none )



-- Until we actually show something different to a no op !


view : Model -> Html Msg
view model =
    div [] [ text "This is the super home page" ]
