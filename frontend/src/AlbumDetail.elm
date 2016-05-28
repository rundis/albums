module AlbumDetail exposing (Model, Msg, init, initForArtist, view, update, mountAlbumCmd, mountNewAlbumCmd)

import TrackRow
import ServerApi exposing (Album, Track, AlbumRequest, Artist, getAlbum, updateAlbum, createAlbum, getArtists)
import Routes
import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, targetValue)
import List.Extra as ListX
import Http


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


type Msg
    = ShowAlbum Album
    | NewAlbum
    | HandleArtistsRetrieved (List Artist)
    | FetchArtistsFailed Http.Error
    | SetAlbumName (String)
    | SaveAlbum
    | HandleSaved Album
    | SaveFailed Http.Error
    | ModifyTrack TrackRowId TrackRow.Msg
    | RemoveTrack TrackRowId
    | MoveTrackUp TrackRowId
    | MoveTrackDown TrackRowId
    | FetchAlbumFailed Http.Error


init : Model
init =
    Model Nothing Nothing "" [] 0 []


initForArtist : Int -> Model
initForArtist artistId =
    Model Nothing (Just artistId) "" [] 0 [] |> maybeAddPristine


mountAlbumCmd : Int -> Cmd Msg
mountAlbumCmd id =
    Cmd.batch
        [ getAlbum id FetchAlbumFailed ShowAlbum
        , getArtists FetchArtistsFailed HandleArtistsRetrieved
        ]


mountNewAlbumCmd : Cmd Msg
mountNewAlbumCmd =
    getArtists FetchArtistsFailed HandleArtistsRetrieved


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchAlbumFailed err ->
            ( model, Cmd.none )

        ShowAlbum album ->
            ( createAlbumModel model album, Cmd.none )

        NewAlbum ->
            ( maybeAddPristine model, getArtists FetchArtistsFailed HandleArtistsRetrieved )

        HandleArtistsRetrieved artists' ->
            ( { model | artists = artists' }
            , Cmd.none
            )

        -- TODO: show error
        FetchArtistsFailed err ->
            ( model, Cmd.none )

        SetAlbumName txt ->
            ( { model | name = txt }
            , Cmd.none
            )

        SaveAlbum ->
            case ( model.id, model.artistId ) of
                ( Just albumId, Just artistId ) ->
                    ( model
                    , updateAlbum (Album albumId model.name artistId (createTracks model.tracks)) SaveFailed HandleSaved
                    )

                ( Nothing, Just artistId ) ->
                    ( model
                    , createAlbum
                        { name = model.name
                        , artistId = artistId
                        , tracks = (createTracks model.tracks)
                        }
                        SaveFailed
                        HandleSaved
                    )

                ( _, _ ) ->
                    Debug.crash "Missing artist.id, needs to be handled by validation"

        HandleSaved album ->
            ( createAlbumModel model album
            , Routes.redirect (Routes.ArtistDetailPage album.artistId)
            )

        SaveFailed err ->
            ( model, Cmd.none )

        RemoveTrack id ->
            ( { model | tracks = List.filter (\( rowId, _ ) -> rowId /= id) model.tracks }
            , Cmd.none
            )

        MoveTrackUp id ->
            let
                track =
                    ListX.find (\( rowId, _ ) -> rowId == id) model.tracks
            in
                case track of
                    Nothing ->
                        ( model, Cmd.none )

                    Just t ->
                        ( { model | tracks = moveUp model.tracks t }
                        , Cmd.none
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
                        ( model, Cmd.none )

                    Just t ->
                        ( { model
                            | tracks =
                                if (mayMoveDown t) then
                                    moveDown model.tracks t
                                else
                                    model.tracks
                          }
                        , Cmd.none
                        )

        ModifyTrack id trackRowMsg ->
            case (updateTrackRow id trackRowMsg model) of
                Just ( updTrack, Nothing ) ->
                    ( maybeAddPristine
                        { model
                            | tracks =
                                ListX.replaceIf (\( i, _ ) -> i == id)
                                    ( id, updTrack )
                                    model.tracks
                        }
                    , Cmd.none
                    )

                Just ( _, Just dispatchMsg ) ->
                    handleDispatch id dispatchMsg model

                Nothing ->
                    ( model, Cmd.none )


updateTrackRow : TrackRowId -> TrackRow.Msg -> Model -> Maybe ( TrackRow.Model, Maybe TrackRow.DispatchMsg )
updateTrackRow id msg model =
    ListX.find (\( trackId, _ ) -> id == trackId) model.tracks
        |> Maybe.map (\( _, trackModel ) -> TrackRow.update msg trackModel)


handleDispatch : TrackRowId -> TrackRow.DispatchMsg -> Model -> ( Model, Cmd Msg )
handleDispatch id msg model =
    case msg of
        TrackRow.MoveDown ->
            update (MoveTrackDown id) model

        TrackRow.MoveUp ->
            update (MoveTrackUp id) model

        TrackRow.Remove ->
            update (RemoveTrack id) model


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


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text <| pageTitle model ]
        , Html.form [ class "form-horizontal" ]
            [ div [ class "form-group" ]
                [ label [ class "col-sm-2 control-label" ] [ text "Name" ]
                , div [ class "col-sm-10" ]
                    [ input
                        [ class "form-control"
                        , value model.name
                        , onInput SetAlbumName
                        ]
                        []
                    ]
                ]
            , (artistDropDown model)
            , div [ class "form-group" ]
                [ div [ class "col-sm-offset-2 col-sm-10" ]
                    [ button
                        [ class "btn btn-default"
                        , type' "button"
                        , onClick SaveAlbum
                        ]
                        [ text "Saves" ]
                    ]
                ]
            ]
        , h2 [] [ text "Tracks" ]
        , trackListing model
        ]


artistDropDown : Model -> Html Msg
artistDropDown model =
    let
        val =
            Maybe.withDefault (-1) model.artistId

        opt a =
            option [ value <| toString a.id, selected (a.id == val) ] [ text a.name ]
    in
        div [ class "form-group" ]
            [ label [ class "col-sm-2 control-label" ] [ text "Artist" ]
            , div [ class "col-sm-10" ]
                [ select [ class "form-control" ]
                    (List.map opt model.artists)
                ]
            ]


trackListing : Model -> Html Msg
trackListing model =
    table [ class "table table-striped" ]
        [ thead []
            [ tr []
                [ th [] []
                , th [] []
                , th [] [ text "Name" ]
                , th [] [ text "Duration" ]
                , th [] []
                ]
            ]
        , tbody [] (List.map trackRow model.tracks)
        ]


trackRow : ( TrackRowId, TrackRow.Model ) -> Html Msg
trackRow ( id, rowModel ) =
    Html.App.map (ModifyTrack id) (TrackRow.view rowModel)



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
