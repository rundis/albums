module ArtistDetail exposing (Model, Msg(..), init, view, update)

import ServerApi exposing (Artist, ArtistRequest, Album, getArtist, updateArtist, createArtist, getAlbumsByArtist, deleteAlbum)
import Routes
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, targetValue)
import Http


type alias Model =
    { id : Maybe Int
    , name : String
    , albums : List Album
    }


type Msg
    = NoOp
    | GetArtist Int
    | FetchArtistFailed Http.Error
    | ShowArtist Artist
    | NewArtist
    | SetArtistName String
    | SaveArtist
    | HandleSaved Artist
    | SaveFailed Http.Error
    | HandleAlbumsRetrieved (List Album)
    | FetchAlbumsFailed Http.Error
    | DeleteAlbum (Int)
    | HandleAlbumDeleted
    | DeleteFailed


init : Model
init =
    Model Nothing "" []


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NoOp ->
            ( model, Cmd.none )

        GetArtist id ->
            ( model, getArtist id FetchArtistFailed ShowArtist )

        -- TODO: Show error
        FetchArtistFailed err ->
            ( model, Cmd.none )

        -- TODO: Show error
        FetchAlbumsFailed err ->
            ( model, Cmd.none )

        ShowArtist artist ->
            ( { model
                | id = Just artist.id
                , name = artist.name
              }
            , getAlbumsByArtist artist.id FetchAlbumsFailed HandleAlbumsRetrieved
            )

        NewArtist ->
            ( { model
                | id = Nothing
                , name = ""
              }
            , Cmd.none
            )

        SaveArtist ->
            case model.id of
                Just id ->
                    ( model, updateArtist (Artist id model.name) SaveFailed HandleSaved )

                Nothing ->
                    ( model, createArtist { name = model.name } SaveFailed HandleSaved )

        HandleSaved artist ->
            ( { model
                | id = Just artist.id
                , name = artist.name
              }
            , Routes.redirect Routes.ArtistListingPage
            )

        SaveFailed err ->
            ( model, Cmd.none )

        SetArtistName txt ->
            ( { model | name = txt }
            , Cmd.none
            )

        HandleAlbumsRetrieved albums' ->
            ( { model | albums = albums' }
            , Cmd.none
            )

        DeleteAlbum id ->
            ( model, deleteAlbum id DeleteFailed HandleAlbumDeleted )

        HandleAlbumDeleted ->
            case model.id of
                Nothing ->
                    ( model, Cmd.none )

                Just id ->
                    ( model, getAlbumsByArtist id FetchAlbumsFailed HandleAlbumsRetrieved )

        -- Show generic error
        DeleteFailed ->
            ( model, Cmd.none )


pageTitle : Model -> String
pageTitle model =
    case model.id of
        Just x ->
            "Edit artists"

        Nothing ->
            "New artist"


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
                        , onInput SetArtistName
                        ]
                        []
                    ]
                ]
            , div [ class "form-group" ]
                [ div [ class "col-sm-offset-2 col-sm-10" ]
                    [ button
                        [ class "btn btn-default"
                        , type' "button"
                        , onClick SaveArtist
                        ]
                        [ text "Save" ]
                    ]
                ]
            ]
        , h2 [] [ text "Albums" ]
        , newAlbumButton model
        , albumListing model
        ]


newAlbumButton : Model -> Html Msg
newAlbumButton model =
    case model.id of
        Nothing ->
            button [ class "pull-right btn btn-default disabled" ] [ text "New Album" ]

        Just x ->
            a
                [ class "pull-right btn btn-default"
                , href <| Routes.encode <| Routes.NewArtistAlbumPage x
                ]
                [ text "New Album" ]


albumListing : Model -> Html Msg
albumListing model =
    table [ class "table table-striped" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Tracks" ]
                , th [] [ text "Length" ]
                , th [] []
                , th [] []
                ]
            ]
        , tbody [] (List.map (albumRow) model.albums)
        ]


albumRow : Album -> Html Msg
albumRow album =
    tr []
        [ td [] [ text album.name ]
        , td [] [ text (toString (List.length album.tracks)) ]
        , td [] [ text (albumTime album) ]
        , td []
            [ a
                [ class "btn btn-sm btn-default"
                , href <| Routes.encode <| Routes.AlbumDetailPage album.id
                ]
                [ text "Edit" ]
            ]
        , td []
            [ button
                [ class "btn btn-sm btn-danger"
                , onClick (DeleteAlbum album.id)
                ]
                [ text "Delete!" ]
            ]
        ]


albumTime : Album -> String
albumTime album =
    let
        tot =
            List.map .duration album.tracks |> List.sum
    in
        (toString <| tot // 60) ++ ":" ++ (toString <| rem tot 60)
