module Routes exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (href)
import Json.Decode as Json
import Html.Events exposing (on, onClick, onWithOptions)
import UrlParser exposing (Parser, parse, (</>), format, int, oneOf, s, string)
import Navigation exposing (Location)


type Route
    = Home
    | ArtistListingPage
    | ArtistDetailPage Int
    | NewArtistPage
    | AlbumDetailPage Int
    | NewAlbumPage
    | NewArtistAlbumPage Int
    | EmptyRoute



--routeParser : List (Matcher Route)


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ format Home (s "/")
        , format ArtistListingPage (s "/artists")
        , format NewArtistPage (s "/artists/new")
        , format ArtistDetailPage (s "/artists" </> int)
        , format AlbumDetailPage (s "/albums" </> int)
        , format NewAlbumPage (s "/albums/new")
        , format NewArtistAlbumPage (s "/artists" </> int </> s "albums/new")
        ]


decode : Location -> Result String Route
decode location =
    parse identity routeParser location.pathname


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


redirect : Route -> Cmd msg
redirect route =
    Navigation.newUrl (encode route)



{- redirect : Route -> Effects ()
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
-}
