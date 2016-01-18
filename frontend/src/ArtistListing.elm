module ArtistListing (Model, Action (..), init, view, update) where


import ServerApi exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Http
import Effects exposing (Effects, Never)
import Json.Decode as Json
import TransitRouter



type alias Model =
  { artists : List Artist}


type Action =
    Show
  | HandleArtistsRetrieved (Maybe (List Artist))
  | DeleteArtist (Int)
  | HandleArtistDeleted (Maybe Http.Response)


init : Model
init =
  Model []


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Show ->
      (model, getArtists HandleArtistsRetrieved)

    HandleArtistsRetrieved xs ->
      ( {model | artists = (Maybe.withDefault [] xs) }
      , Effects.none
      )

    DeleteArtist id ->
      (model, deleteArtist id HandleArtistDeleted)

    HandleArtistDeleted res ->
      (model, getArtists HandleArtistsRetrieved)



------ VIEW ------

artistRow : Signal.Address Action -> Artist -> Html
artistRow address artist =
  tr [] [
     td [] [text artist.name]
    ,td [] [button [ on "click" Json.value (\_ ->  Signal.message TransitRouter.pushPathAddress ("/artist/" ++ (toString artist.id) ))] [text "Edit"]]
    ,td [] [button [ onClick address (DeleteArtist (.id artist))] [ text "Delete!" ]]
  ]


view : Signal.Address Action -> Model -> Html
view address model =
  div [] [
      h1 [] [text "Artists" ]
    , button [
          class "pull-right btn btn-default"
        ]
        [text "New Artist"]
    , table [class "table table-striped"] [
          thead [] [
            tr [] [
               th [] [text "Name"]
              ,th [] []
              ,th [] []
          ]
        ]
        , tbody [] (List.map (artistRow address) model.artists)
    ]
  ]

