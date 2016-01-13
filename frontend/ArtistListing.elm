module ArtistListing (Model, Action (..), init, view, update) where


import ServerApi exposing (..)
import ArtistDetail
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Http
import Effects exposing (Effects, Never)


type Page = ArtistListingPage | ArtistDetailPage


type alias Model =
  { artists : List Artist
  , artistDetail : ArtistDetail.Model
  , page : Page}


type Action =
    HandleArtistsRetrieved (Maybe (List Artist))
  | SelectArtist (Int)
  | DeleteArtist (Int)
  | HandleArtistDeleted (Maybe Http.Response)
  | ArtistDetailAction ArtistDetail.Action
  | NewArtist


init : (Model, Effects Action)
init =
  let
    (artistDetail, fx) = ArtistDetail.init
  in
    ( Model [] artistDetail ArtistListingPage
      , getArtists HandleArtistsRetrieved
    )


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of

    HandleArtistsRetrieved xs ->
      ( {model | artists = (Maybe.withDefault [] xs) }
      , Effects.none
      )

    DeleteArtist id ->
      (model, deleteArtist id HandleArtistDeleted)

    HandleArtistDeleted res ->
      (model, getArtists HandleArtistsRetrieved)

    NewArtist ->
      update (ArtistDetailAction <| ArtistDetail.ShowArtist Nothing) model

    SelectArtist id ->
      update (ArtistDetailAction <| ArtistDetail.GetArtist id) model

    ArtistDetailAction sub ->
      let
        (detailModel, fx) = ArtistDetail.update sub model.artistDetail
      in
        ( { model | artistDetail = detailModel
                  , page = ArtistDetailPage }
        , Effects.map ArtistDetailAction fx -- handle fx !
        )


------ VIEW ------

artistRow : Signal.Address Action -> Artist -> Html
artistRow address artist =
  tr [] [
     td [] [text artist.name]
    ,td [] [button [ onClick address (SelectArtist artist.id) ] [text "Edit"]]
    ,td [] [button [ onClick address (DeleteArtist (.id artist))] [ text "Delete!" ]]
  ]


artistsView : Signal.Address Action -> Model -> Html
artistsView address model =
  div [] [
      h1 [] [text "Artists" ]
    , button [
          class "pull-right btn btn-default"
        , onClick address NewArtist]
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


view : Signal.Address Action -> Model -> Html
view address model =
  div [class "content"] [
    case model.page of

      ArtistListingPage ->
        artistsView address model

      ArtistDetailPage ->
        ArtistDetail.view (Signal.forwardTo address ArtistDetailAction) model.artistDetail

  ]

