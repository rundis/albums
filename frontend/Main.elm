module Main where


import ArtistListing
import ArtistDetail
import Home
import Routes exposing (..)
import ServerApi
import Html exposing (..)
import Html.Attributes exposing (..)
import Task exposing (..)
import Effects exposing (Effects, Never)
import Signal exposing (message)
import StartApp
import TransitRouter exposing (WithRoute, getTransition)
import TransitStyle



type alias Model = WithRoute Routes.Route
  { homeModel : Home.Model
  , artistListingModel : ArtistListing.Model
  , artistDetailModel : ArtistDetail.Model
  }


type Action =
    NoOp
  | HomeAction Home.Action
  | ArtistListingAction ArtistListing.Action
  | ArtistDetailAction ArtistDetail.Action
  | RouterAction (TransitRouter.Action Routes.Route)




initialModel : Model
initialModel =
  { transitRouter = TransitRouter.empty Routes.EmptyRoute
  , homeModel = Home.init
  , artistListingModel = ArtistListing.init
  , artistDetailModel = ArtistDetail.init
  }


actions : Signal Action
actions =
  Signal.map RouterAction TransitRouter.actions


mountRoute : Route -> Route -> Model -> (Model, Effects Action)
mountRoute prevRoute route model =
  case route of

    Home ->
      (model, Effects.none)

    ArtistListingPage ->
      (model, Effects.map ArtistListingAction (ServerApi.getArtists ArtistListing.HandleArtistsRetrieved))

    ArtistDetailPage artistId ->
      (model, Effects.map ArtistDetailAction (ServerApi.getArtist artistId ArtistDetail.ShowArtist))

    NewArtistPage ->
      ({ model | artistDetailModel = ArtistDetail.init } , Effects.none)

    EmptyRoute ->
      (model, Effects.none)


routerConfig : TransitRouter.Config Routes.Route Action Model
routerConfig =
  { mountRoute = mountRoute
  , getDurations = \_ _ _ -> (50, 200)
  , actionWrapper = RouterAction
  , routeDecoder = Routes.decode
  }


init : String -> (Model, Effects Action)
init path =
  TransitRouter.init routerConfig path initialModel



update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of

    NoOp ->
      (model, Effects.none)

    HomeAction homeAction ->
      let (model', effects) = Home.update homeAction model.homeModel
      in ( { model | homeModel = model' }
         , Effects.map HomeAction effects )

    ArtistListingAction act ->
      let (model', effects) = ArtistListing.update act model.artistListingModel
      in ( { model | artistListingModel = model' }
         , Effects.map ArtistListingAction effects )

    ArtistDetailAction act ->
      let (model', effects) = ArtistDetail.update act model.artistDetailModel
      in ( { model | artistDetailModel = model' }
         , Effects.map ArtistDetailAction effects )

    RouterAction routeAction ->
      TransitRouter.update routerConfig routeAction model





-- Main view/layout functions

menu : Signal.Address Action -> Model -> Html
menu address model =
  header [class "navbar navbar-default"] [
    div [class "container"] [
        div [class "navbar-header"] [
          div [ class "navbar-brand" ] [
            a (linkAttrs Home) [ text "Albums galore" ]
          ]
        ]
      , ul [class "nav navbar-nav"] [
          li [] [a (linkAttrs ArtistListingPage) [ text "Artists" ]]
      ]
    ]
  ]




contentView : Signal.Address Action -> Model -> Html
contentView address model =
  case (TransitRouter.getRoute model) of
    Home ->
      Home.view (Signal.forwardTo address HomeAction) model.homeModel

    ArtistListingPage ->
      ArtistListing.view (Signal.forwardTo address ArtistListingAction) model.artistListingModel

    ArtistDetailPage i ->
      ArtistDetail.view (Signal.forwardTo address ArtistDetailAction) model.artistDetailModel

    NewArtistPage  ->
      ArtistDetail.view (Signal.forwardTo address ArtistDetailAction) model.artistDetailModel

    EmptyRoute ->
      text "Empty WHAT ?"


view : Signal.Address Action -> Model -> Html
view address model =
  div [class "container-fluid"] [
      menu address model
    , div [ class "content"
          , style (TransitStyle.fadeSlideLeft 100 (getTransition model))]
          [contentView address model]
  ]



-- wiring up start app

app : StartApp.App Model
app =
  StartApp.start
    { init = init initialPath
    , update = update
    , view = view
    , inputs = [actions]
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


port initialPath : String

