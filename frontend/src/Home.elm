module Home exposing (..)

import Signal
import Html exposing (div, text, Html)
import Effects exposing (Effects)


type alias Model =
    ()


init : Model
init =
    ()


type Action
    = NoOp
    | Show


update : Action -> Model -> ( Model, Effects Action )
update action model =
    case action of
        NoOp ->
            ( model
            , Effects.none
            )

        Show ->
            update action model



-- Until we actually show something different to a no op !


view : Signal.Address Action -> Model -> Html
view address model =
    div [] [ text "This is the super home page" ]
