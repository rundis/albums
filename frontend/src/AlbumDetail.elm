module AlbumDetail (Model, Action(..), init, initForArtist, view, update) where

import TrackRow
import ServerApi exposing (Album, Track, AlbumRequest, Artist, getAlbum, updateAlbum, createAlbum, getArtists)
import Routes
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import List.Extra as ListX


type alias Model =
  { id : Maybe Int
  , artistId : Maybe Int
  , name : String
  , tracks : List ( TrackRowId, TrackRow.Model )
  , nextTrackRowId : TrackRowId
  , artists : List Artist
  }


type alias TrackRowId =
  Int


type Action
  = NoOp
  | GetAlbum (Int)
  | ShowAlbum (Maybe Album)
  | HandleArtistsRetrieved (Maybe (List Artist))
  | SetAlbumName (String)
  | SaveAlbum
  | HandleSaved (Maybe Album)
  | ModifyTrack TrackRowId TrackRow.Action
  | RemoveTrack TrackRowId
  | MoveTrackUp TrackRowId
  | MoveTrackDown TrackRowId


init : Model
init =
  Model Nothing Nothing "" [] 0 []

initForArtist : Int -> Model
initForArtist artistId =
  Model Nothing (Just artistId) "" [] 0 []


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    GetAlbum id ->
      ( model
      , Effects.batch
          [ getAlbum id ShowAlbum
          , getArtists HandleArtistsRetrieved
          ]
      )

    ShowAlbum maybeAlbum ->
      case maybeAlbum of
        Just album ->
          ( createAlbumModel model album, Effects.none )

        Nothing -> -- TODO: This could be an error if returned from api !
          ( maybeAddPristine model, getArtists HandleArtistsRetrieved )

    HandleArtistsRetrieved xs ->
      ( { model | artists = (Maybe.withDefault [] xs) }
      , Effects.none
      )

    SetAlbumName txt ->
      ( { model | name = txt }
      , Effects.none
      )

    SaveAlbum ->
      case (model.id, model.artistId) of
        (Just albumId, Just artistId) ->
          ( model
          , updateAlbum (Album albumId model.name artistId (createTracks model.tracks)) HandleSaved
          )
        (Nothing, Just artistId) ->
          ( model
          , createAlbum { name = model.name
                          , artistId = artistId
                          , tracks = (createTracks model.tracks)
                          } HandleSaved
          )
        (_, _) ->
          Debug.crash "Missing artist.id, needs to be handled by validation"


    HandleSaved maybeAlbum ->
      case maybeAlbum of
        Just album ->
          ( createAlbumModel model album
          , Effects.map (\_ -> NoOp) (Routes.redirect <| Routes.ArtistDetailPage album.artistId)
          )

        Nothing ->
          Debug.crash "Save failed... we're not handling it..."

    RemoveTrack id ->
      ( { model | tracks = List.filter (\( rowId, _ ) -> rowId /= id) model.tracks }
      , Effects.none
      )

    MoveTrackUp id ->
      let
        track =
          ListX.find (\( rowId, _ ) -> rowId == id) model.tracks
      in
        case track of
          Nothing ->
            ( model, Effects.none )

          Just t ->
            ( { model | tracks = moveUp model.tracks t }
            , Effects.none
            )

    MoveTrackDown id ->
      let
        track =
          ListX.find (\( rowId, _ ) -> rowId == id) model.tracks

        mayMoveDown t =
          let
            idx =
              ListX.elemIndex t model.tracks
          in
            case idx of
              Nothing ->
                False

              Just i ->
                i < ((List.length model.tracks) - 2)
      in
        case track of
          Nothing ->
            ( model, Effects.none )

          Just t ->
            ( { model
                | tracks =
                    if (mayMoveDown t) then
                      moveDown model.tracks t
                    else
                      model.tracks
              }
            , Effects.none
            )

    ModifyTrack id trackRowAction ->
      let
        updateTrack ( trackId, trackModel ) =
          if trackId == id then
            ( trackId, TrackRow.update trackRowAction trackModel )
          else
            ( trackId, trackModel )
      in
        ( maybeAddPristine { model | tracks = List.map updateTrack model.tracks }
        , Effects.none
        )


createAlbumModel : Model -> Album -> Model
createAlbumModel model album =
  { id = Just album.id
  , artistId = Just album.artistId
  , name = album.name
  , tracks = createTrackRowModels album.tracks
  , nextTrackRowId = (List.length album.tracks) + 2
  , artists = model.artists
  }


createTrackRowModels : List Track -> List ( TrackRowId, TrackRow.Model )
createTrackRowModels tracks =
  let
    saved =
      List.indexedMap (\i t -> ( i, TrackRow.init t.name (Just t.duration) )) tracks
  in
    List.append saved [ ( List.length saved, TrackRow.initPristine ) ]


createTracks : List ( TrackRowId, TrackRow.Model ) -> List Track
createTracks rows =
  let
    calcDuration r =
      (Maybe.withDefault 0 r.durationMin * 60) + (Maybe.withDefault 0 r.durationSec)
  in
    List.take ((List.length rows) - 1) rows
      |> List.map (\( _, r ) -> Track r.name (calcDuration r))


maybeAddPristine : Model -> Model
maybeAddPristine model =
  if (List.any (\( _, r ) -> TrackRow.isPristine r) model.tracks) then
    model
  else
    { model
      | nextTrackRowId = model.nextTrackRowId + 1
      , tracks = List.append model.tracks [ ( model.nextTrackRowId, TrackRow.initPristine ) ]
    }


pageTitle : Model -> String
pageTitle model =
  case model.id of
    Just x ->
      "Edit album"

    Nothing ->
      "New album"


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ h1 [] [ text <| pageTitle model ]
    , Html.form
        [ class "form-horizontal" ]
        [ div
            [ class "form-group" ]
            [ label [ class "col-sm-2 control-label" ] [ text "Name" ]
            , div
                [ class "col-sm-10" ]
                [ input
                    [ class "form-control"
                    , value model.name
                    , on "input" targetValue (\str -> Signal.message address (SetAlbumName str))
                    ]
                    []
                ]
            ]
        , ( artistDropDown address model )
        , div
            [ class "form-group" ]
            [ div
                [ class "col-sm-offset-2 col-sm-10" ]
                [ button
                    [ class "btn btn-default"
                    , type' "button"
                    , onClick address SaveAlbum
                    ]
                    [ text "Save" ]
                ]
            ]
        ]
    , h2 [] [ text "Tracks" ]
    , trackListing address model
    ]


artistDropDown : Signal.Address Action -> Model -> Html
artistDropDown address model =
  let
    val =
      Maybe.withDefault (-1) model.artistId

    opt a =
        option [ value <| toString a.id, selected (a.id == val) ] [ text a.name ]
  in
    div
      [ class "form-group" ]
      [ label [ class "col-sm-2 control-label" ] [ text "Artist" ]
      , div
          [ class "col-sm-10" ]
          [ select
              [ class "form-control" ]
              (List.map opt model.artists)
          ]
      ]


trackListing : Signal.Address Action -> Model -> Html
trackListing address model =
  table
    [ class "table table-striped" ]
    [ thead
        []
        [ tr
            []
            [ th [] []
            , th [] []
            , th [] [ text "Name" ]
            , th [] [ text "Duration" ]
            , th [] []
            ]
        ]
    , tbody [] (List.map (trackRow address) model.tracks)
    ]


trackRow : Signal.Address Action -> ( TrackRowId, TrackRow.Model ) -> Html
trackRow address ( id, rowModel ) =
  let
    context =
      TrackRow.Context
        (Signal.forwardTo address (ModifyTrack id))
        (Signal.forwardTo address (always (RemoveTrack id)))
        (Signal.forwardTo address (always (MoveTrackUp id)))
        (Signal.forwardTo address (always (MoveTrackDown id)))
  in
    TrackRow.view context rowModel



-- list utils


moveUp : List a -> a -> List a
moveUp elems elem =
  let
    idx =
      ListX.elemIndex elem elems
  in
    case idx of
      Nothing ->
        elems

      Just x ->
        let
          ( a, b ) =
            ListX.splitAt (x - 1) elems
        in
          List.concat [ a, [ elem ], (ListX.removeWhen ((==) elem) b) ]


moveDown : List a -> a -> List a
moveDown elems elem =
  let
    idx =
      ListX.elemIndex elem elems
  in
    case idx of
      Nothing ->
        elems

      Just x ->
        let
          ( a, b ) =
            ListX.splitAt (x + 2) elems
        in
          List.concat [ (ListX.removeWhen ((==) elem) a), [ elem ], b ]

