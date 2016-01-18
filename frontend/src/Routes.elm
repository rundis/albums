module Routes where

import Effects exposing (Effects)
import RouteParser exposing (..)
import TransitRouter
import Html exposing (Attribute)
import Html.Attributes exposing (href)
import Json.Decode as Json
import Html.Events exposing (onClick, onWithOptions)
import Signal exposing (message)


type Route
  = Home
  | ArtistListingPage
  | ArtistDetailPage Int
  | EmptyRoute


routeParsers : List (Matcher Route)
routeParsers =
  [ static Home "/"
  , static ArtistListingPage "/artists"
  , dyn1 ArtistDetailPage "/artists/" int ""
  ]


decode : String -> Route
decode path =
  RouteParser.match routeParsers path
    |> Maybe.withDefault EmptyRoute


encode : Route -> String
encode route =
  case route of
    Home -> "/"
    ArtistListingPage   -> "/artists"
    ArtistDetailPage  i -> "/artists/" ++ toString i
    EmptyRoute -> ""



redirect : Route -> Effects ()
redirect route =
  encode route
    |> Signal.send TransitRouter.pushPathAddress
    |> Effects.task


clickTo : String -> List Attribute
clickTo path =
  [ href path
  , onWithOptions
      "click"
      { stopPropagation = True, preventDefault = True }
      Json.value
      (\_ ->  message TransitRouter.pushPathAddress path)
  ]

-- (Debug.log ("Gotos: " ++ toString (decode path)))
