module Routes exposing (..)

import UrlParser exposing (Parser, parse, (</>), format, int, oneOf, s, string)
import Navigation exposing (Location)
import String
import Html.Attributes exposing (href, attribute)
import Html exposing (Html, Attribute, a)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json
import Json.Decode.Extra exposing (lazy)


type Route
    = Home
    | ArtistListingPage
    | ArtistDetailPage Int
    | NewArtistPage
    | AlbumDetailPage Int
    | NewArtistAlbumPage Int
    | EmptyRoute



--routeParser : List (Matcher Route)


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ format Home (s "")
        , format NewArtistPage (s "artists" </> s "new")
        , format NewArtistAlbumPage (s "artists" </> int </> s "albums" </> s "new")
        , format ArtistDetailPage (s "artists" </> int)
        , format ArtistListingPage (s "artists")
        , format AlbumDetailPage (s "albums" </> int)
        ]



--parse identity routeParser "artists"


decode : Location -> Result String Route
decode location =
    parse identity routeParser (String.dropLeft 1 location.pathname)


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

        EmptyRoute ->
            ""


redirect : Route -> Cmd msg
redirect route =
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
        [ Json.at [ "dataset", "navigate" ] Json.string
        , Json.at [ "data-navigate" ] Json.string
        , Json.at [ "parentElement" ] (lazy (\_ -> pathDecoder))
        , Json.fail "no path found for click"
        ]
