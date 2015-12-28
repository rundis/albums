module Album where


import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import Effects exposing (Effects, Never)
import StartApp



type alias Artist =
  { id : Int
  , name : String
  }

type alias Model =
  { artists : List Artist}


type Action = ArtistRetrieved (Maybe (List Artist))


init : (Model, Effects Action)
init =
  ( Model []
    , getArtists
  )


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    ArtistRetrieved xs ->
      ( {model | artists = (Maybe.withDefault [] xs) }
      , Effects.none
      )


getArtists : Effects.Effects Action
getArtists =
  Http.get artists "http://localhost:8081/artists"
    |> Task.toMaybe
    |> Task.map ArtistRetrieved
    |> Effects.task


artist : Json.Decoder Artist
artist =
  Json.object2 Artist
    ("artistId" := Json.int)
    ("name" := Json.string)


artists : Json.Decoder (List Artist)
artists =
  Json.list artist



artistRow : Artist -> Html
artistRow artist =
  tr [] [
     td [] [text (toString artist.id)]
    ,td [] [text artist.name]
  ]

view : Signal.Address Action -> Model -> Html
view address model =
  div [class "container-fluid"] [
        h1 [] [text "Artists" ]
      , table [class "table table-striped"] [
          thead [] [
            tr [] [
               th [] [text "Id"]
              ,th [] [text "Name"]
          ]
        ]
      , tbody [] (List.map artistRow model.artists)
    ]
  ]




app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }



main : Signal Html
main =
  app.html



port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
