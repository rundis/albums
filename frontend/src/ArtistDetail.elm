module ArtistDetail exposing (Model, Action(..), init, view, update)

import ServerApi exposing (Artist, ArtistRequest, Album, getArtist, updateArtist, createArtist, getAlbumsByArtist, deleteAlbum)
import Routes
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Http
import Debug


type alias Model =
    { id : Maybe Int
    , name : String
    , albums : List Album
    }


type Action
    = NoOp
    | GetArtist (Int)
    | ShowArtist (Maybe Artist)
    | SetArtistName (String)
    | SaveArtist
    | HandleSaved (Maybe Artist)
    | HandleAlbumsRetrieved (Maybe (List Album))
    | DeleteAlbum (Int)
    | HandleAlbumDeleted (Maybe Http.Response)


init : Model
init =
    Model Nothing "" []


update : Action -> Model -> ( Model, Effects Action )
update action model =
    case action of
        NoOp ->
            ( model, Effects.none )

        GetArtist id ->
            ( model, getArtist id ShowArtist )

        ShowArtist maybeArtist ->
            case maybeArtist of
                Just artist ->
                    ( { model
                        | id = Just artist.id
                        , name = artist.name
                      }
                    , getAlbumsByArtist artist.id HandleAlbumsRetrieved
                    )

                -- TODO: This could be an error if returned from api !
                Nothing ->
                    ( { model
                        | id = Nothing
                        , name = ""
                      }
                    , Effects.none
                    )

        SaveArtist ->
            case model.id of
                Just id ->
                    ( model, updateArtist (Artist id model.name) HandleSaved )

                Nothing ->
                    ( model, createArtist { name = model.name } HandleSaved )

        HandleSaved maybeArtist ->
            case maybeArtist of
                Just artist ->
                    ( { model
                        | id = Just artist.id
                        , name = artist.name
                      }
                    , Effects.map (\_ -> NoOp) (Routes.redirect Routes.ArtistListingPage)
                    )

                Nothing ->
                    Debug.crash "Save failed... we're not handling it..."

        SetArtistName txt ->
            ( { model | name = txt }
            , Effects.none
            )

        HandleAlbumsRetrieved xs ->
            ( { model | albums = (Maybe.withDefault [] xs) }
            , Effects.none
            )

        DeleteAlbum id ->
            ( model, deleteAlbum id HandleAlbumDeleted )

        HandleAlbumDeleted res ->
            case model.id of
                Nothing ->
                    ( model, Effects.none )

                Just id ->
                    ( model, getAlbumsByArtist id HandleAlbumsRetrieved )


pageTitle : Model -> String
pageTitle model =
    case model.id of
        Just x ->
            "Edit artists"

        Nothing ->
            "New artist"


view : Signal.Address Action -> Model -> Html
view address model =
    div []
        [ h1 [] [ text <| pageTitle model ]
        , Html.form [ class "form-horizontal" ]
            [ div [ class "form-group" ]
                [ label [ class "col-sm-2 control-label" ] [ text "Name" ]
                , div [ class "col-sm-10" ]
                    [ input
                        [ class "form-control"
                        , value model.name
                        , on "input" targetValue (\str -> Signal.message address (SetArtistName str))
                        ]
                        []
                    ]
                ]
            , div [ class "form-group" ]
                [ div [ class "col-sm-offset-2 col-sm-10" ]
                    [ button
                        [ class "btn btn-default"
                        , type' "button"
                        , onClick address SaveArtist
                        ]
                        [ text "Save" ]
                    ]
                ]
            ]
        , h2 [] [ text "Albums" ]
        , newAlbumButton model
        , albumListing address model
        ]


newAlbumButton : Model -> Html
newAlbumButton model =
    case model.id of
        Nothing ->
            button [ class "pull-right btn btn-default disabled" ] [ text "New Album" ]

        Just x ->
            button
                [ class "pull-right btn btn-default"
                , Routes.clickAttr (Routes.NewArtistAlbumPage x)
                ]
                [ text "New Album" ]


albumListing : Signal.Address Action -> Model -> Html
albumListing address model =
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
        , tbody [] (List.map (albumRow address) model.albums)
        ]


albumRow : Signal.Address Action -> Album -> Html
albumRow address album =
    tr []
        [ td [] [ text album.name ]
        , td [] [ text (toString (List.length album.tracks)) ]
        , td [] [ text (albumTime album) ]
        , td []
            [ button
                [ class "btn btn-sm btn-default"
                , Routes.clickAttr <| Routes.AlbumDetailPage album.id
                ]
                [ text "Edit" ]
            ]
        , td []
            [ button
                [ class "btn btn-sm btn-danger"
                , onClick address (DeleteAlbum album.id)
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
