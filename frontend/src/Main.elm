module Main exposing (..)

import ArtistListing
import ArtistDetail
import AlbumDetail
import Home
import Routes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html
import Navigation


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { route : Routes.Route
    , homeModel : Home.Model
    , artistListingModel : ArtistListing.Model
    , artistDetailModel : ArtistDetail.Model
    , albumDetailModel : AlbumDetail.Model
    }


type Msg
    = HomeMsg Home.Msg
    | ArtistListingMsg ArtistListing.Msg
    | ArtistDetailMsg ArtistDetail.Msg
    | AlbumDetailMsg AlbumDetail.Msg
    | Navigate String
    | UrlChange Navigation.Location


initialModel : Model
initialModel =
    { route = Home
    , homeModel = Home.init
    , artistListingModel = ArtistListing.init
    , artistDetailModel = ArtistDetail.init
    , albumDetailModel = AlbumDetail.init
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init loc =
    update (UrlChange loc) initialModel



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HomeMsg m ->
            let
                ( subMdl, subCmd ) =
                    Home.update m model.homeModel
            in
                { model | homeModel = subMdl }
                    ! [ Cmd.map HomeMsg subCmd ]

        ArtistListingMsg m ->
            let
                ( subMdl, subCmd ) =
                    ArtistListing.update m model.artistListingModel
            in
                { model | artistListingModel = subMdl }
                    ! [ Cmd.map ArtistListingMsg subCmd ]

        ArtistDetailMsg m ->
            let
                ( subMdl, subCmd ) =
                    ArtistDetail.update m model.artistDetailModel
            in
                { model | artistDetailModel = subMdl }
                    ! [ Cmd.map ArtistDetailMsg subCmd ]

        AlbumDetailMsg m ->
            let
                ( subMdl, subCmd ) =
                    AlbumDetail.update m model.albumDetailModel
            in
                { model | albumDetailModel = subMdl }
                    ! [ Cmd.map AlbumDetailMsg subCmd ]

        UrlChange loc ->
            urlUpdate loc model

        Navigate url ->
            model ! [ Navigation.newUrl url ]


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate loc model =
    case (Routes.decode loc) of
        Nothing  ->
            model ! [ Navigation.modifyUrl (Routes.encode model.route) ]

        Just (ArtistListingPage as route) ->
            { model | route = route }
                ! [ Cmd.map ArtistListingMsg ArtistListing.mountCmd ]

        Just ((ArtistDetailPage artistId) as route) ->
            { model | route = route }
                ! [ Cmd.map ArtistDetailMsg <| ArtistDetail.mountShowCmd artistId ]

        Just (NewArtistPage as route) ->
            { model | route = route, artistDetailModel = ArtistDetail.init } ! []

        Just ((AlbumDetailPage albumId) as route) ->
            { model | route = route }
                ! [ Cmd.map AlbumDetailMsg <| AlbumDetail.mountAlbumCmd albumId ]

        Just ((NewArtistAlbumPage artistId) as route) ->
            { model
                | route = route
                , albumDetailModel = AlbumDetail.initForArtist artistId
            }
                ! [ Cmd.map AlbumDetailMsg AlbumDetail.mountNewAlbumCmd ]

        Just route ->
            { model | route = route } ! []


view : Model -> Html Msg
view model =
    div
        [ class "container-fluid"
        , Routes.catchNavigationClicks Navigate
        ]
        [ menu model
        , div [ class "content" ]
            [ contentView model ]
        ]


menu : Model -> Html Msg
menu model =
    header [ class "navbar navbar-default" ]
        [ div [ class "container" ]
            [ div [ class "navbar-header" ]
                [ div [ class "navbar-brand" ]
                    [ Routes.linkTo Routes.Home
                        []
                        [ text "Albums galores" ]
                    ]
                ]
            , ul [ class "nav navbar-nav" ]
                [ div [ class "navbar-brand" ]
                    [ Routes.linkTo Routes.ArtistListingPage
                        []
                        [ text "Artists" ]
                    ]
                ]
            ]
        ]


contentView : Model -> Html Msg
contentView model =
    case model.route of
        Home ->
            Html.map HomeMsg <| Home.view model.homeModel

        ArtistListingPage ->
            Html.map ArtistListingMsg <| ArtistListing.view model.artistListingModel

        ArtistDetailPage i ->
            Html.map ArtistDetailMsg <| ArtistDetail.view model.artistDetailModel

        NewArtistPage ->
            Html.map ArtistDetailMsg <| ArtistDetail.view model.artistDetailModel

        AlbumDetailPage i ->
            Html.map AlbumDetailMsg <| AlbumDetail.view model.albumDetailModel

        NewArtistAlbumPage i ->
            Html.map AlbumDetailMsg <| AlbumDetail.view model.albumDetailModel
