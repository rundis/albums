module Main where


import ArtistListing
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task exposing (..)
import Effects exposing (Effects, Never)
import StartApp


type alias Model =
  { artistListing : ArtistListing.Model}


type Action =
    ShowHomePage
  | ArtistListingAction ArtistListing.Action


init : (Model, Effects Action)
init =
  let
    (artistListing, fx) = ArtistListing.init
  in
    ( Model artistListing
      , Effects.map ArtistListingAction fx
    )




update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of

    ShowHomePage ->
      let
        (artistListing, fx) = ArtistListing.init
      in
        ( {model | artistListing = artistListing}
        , Effects.map ArtistListingAction fx
        )

    ArtistListingAction sub ->
      let
        (artistListing, fx) = ArtistListing.update sub model.artistListing
      in
        ( {model | artistListing = artistListing}
        , Effects.map ArtistListingAction fx
        )


menu : Signal.Address Action -> Model -> Html
menu address model =
  header [class "navbar navbar-default"] [
    div [class "container"] [
      div [class "navbar-header"] [
        button [ class "btn-link navbar-brand", onClick address ShowHomePage ]
        [text "Albums Crud"]
      ]
    ]
  ]


view : Signal.Address Action -> Model -> Html
view address model =
  div [class "container-fluid"] [
      menu address model
    ,  ArtistListing.view (Signal.forwardTo address ArtistListingAction) model.artistListing
  ]


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
