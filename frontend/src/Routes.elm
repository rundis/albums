module Routes exposing (..)

import UrlParser exposing (Parser, (</>), int, oneOf, s)
import Navigation exposing (Location)
import Html.Attributes exposing (href, attribute)
import Html exposing (Html, Attribute, a)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json


type Route
    = Home
    | ArtistListingPage
    | ArtistDetailPage Int
    | NewArtistPage
    | AlbumDetailPage Int
    | NewArtistAlbumPage Int


routeParser : Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home (s "")
        , UrlParser.map NewArtistPage (s "artists" </> s "new")
        , UrlParser.map NewArtistAlbumPage (s "artists" </> int </> s "albums" </> s "new")
        , UrlParser.map ArtistDetailPage (s "artists" </> int)
        , UrlParser.map ArtistListingPage (s "artists")
        , UrlParser.map AlbumDetailPage (s "albums" </> int)
        ]


decode : Location -> Maybe Route
decode location =
    UrlParser.parsePath routeParser location



--parse identity routeParser (String.dropLeft 1 location.pathname)


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

        NewArtistAlbumPage i ->
            "/artists/" ++ (toString i) ++ "/albums/new"


navigate : Route -> Cmd msg
navigate route =
    Navigation.newUrl (encode route)


linkTo : Route -> List (Attribute msg) -> List (Html msg) -> Html msg
linkTo route attrs content =
    a ((linkAttrs route) ++ attrs) content


linkAttrs : Route -> List (Attribute msg)
linkAttrs route =
    let
        path =
            encode route
    in
        [ href path
        , attribute "data-navigate" path
        ]


catchNavigationClicks : (String -> msg) -> Attribute msg
catchNavigationClicks tagger =
    onWithOptions "click"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.map tagger (Json.at [ "target" ] pathDecoder))


pathDecoder : Json.Decoder String
pathDecoder =
    Json.oneOf
        [ Json.at [ "data-navigate" ] Json.string
        , Json.at [ "parentElement" ] (Json.lazy (\_ -> pathDecoder))
        , Json.fail "no path found for click"
        ]
