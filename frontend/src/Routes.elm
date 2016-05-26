module Routes exposing (..)

import Effects exposing (Effects)
import RouteParser exposing (..)
import TransitRouter
import Html exposing (Attribute)
import Html.Attributes exposing (href)
import Json.Decode as Json
import Html.Events exposing (on, onClick, onWithOptions)
import Signal


type Route
    = Home
    | ArtistListingPage
    | ArtistDetailPage Int
    | NewArtistPage
    | AlbumDetailPage Int
    | NewAlbumPage
    | NewArtistAlbumPage Int
    | EmptyRoute


routeParsers : List (Matcher Route)
routeParsers =
    [ static Home "/"
    , static ArtistListingPage "/artists"
    , static NewArtistPage "/artists/new"
    , dyn1 ArtistDetailPage "/artists/" int ""
    , dyn1 AlbumDetailPage "/albums/" int ""
    , static NewAlbumPage "/albums/new"
    , dyn1 NewArtistAlbumPage "/artists/" int "/albums/new"
    ]


decode : String -> Route
decode path =
    RouteParser.match routeParsers path
        |> Maybe.withDefault EmptyRoute


encode : Route -> String
encode route =
    case route of
        Home ->
            "/"

        ArtistListingPage ->
            "/artists"

        NewArtistPage ->
            "/artists/new"

        ArtistDetailPage i ->
            "/artists/" ++ toString i

        AlbumDetailPage i ->
            "/albums/" ++ toString i

        NewAlbumPage ->
            "/albums/new"

        NewArtistAlbumPage i ->
            "/artists/" ++ (toString i) ++ "/albums/new"

        EmptyRoute ->
            ""


redirect : Route -> Effects ()
redirect route =
    encode route
        |> Signal.send TransitRouter.pushPathAddress
        |> Effects.task


clickAttr : Route -> Attribute
clickAttr route =
    on "click" Json.value (\_ -> Signal.message TransitRouter.pushPathAddress <| encode route)


linkAttrs : Route -> List Attribute
linkAttrs route =
    let
        path =
            encode route
    in
        [ href path
        , onWithOptions "click"
            { stopPropagation = True, preventDefault = True }
            Json.value
            (\_ -> Signal.message TransitRouter.pushPathAddress path)
        ]
