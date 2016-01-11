module Album where


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Http
import Json.Decode as Json exposing ((:=))
import Json.Encode as JsonEncode
import Task exposing (..)
import Effects exposing (Effects, Never)
import StartApp
import Debug




type Page = ArtistListing | ArtistDetail

type alias Artist =
  { id : Int
  , name : String
  }



type alias Model =
  { artists : List Artist
  , currArtist : Artist
  , page : Page}


type Action =
    ArtistsRetrieved (Maybe (List Artist))
  | DeleteArtist (Int)
  | ArtistDeleted (Result Http.RawError Http.Response)
  | GetArtist (Int)
  | ArtistRetrieved (Maybe Artist)
  | SetArtistName (String)
  | UpdateArtist
  | ArtistUpdated (Result Http.RawError Http.Response)


init : (Model, Effects Action)
init =
  ( Model [] (Artist 0 "") ArtistListing
    , getArtists
  )



update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    ArtistsRetrieved xs ->
      ( {model | artists = (Maybe.withDefault [] xs) }
      , Effects.none
      )
    DeleteArtist id ->
      Debug.log ("We are here: " ++ toString id ++ " nisse")
      (model, deleteArtist id)
    ArtistDeleted res ->
      -- TODO : Error handling !!
      (model, getArtists)
    GetArtist id ->
      ( { model | page = ArtistDetail}
      , getArtist id
      )
    ArtistRetrieved artist ->
      case artist of
        Just a ->
          ( { model | currArtist = a }
          , Effects.none
          )
        Nothing ->  Debug.crash "Not handled yet"
    SetArtistName txt ->
      let
        artist = model.currArtist
        updArtist = {artist | name = txt}
      in
        ( { model | currArtist = updArtist }
        , Effects.none
        )
    UpdateArtist ->
      (model, updateArtist model.currArtist)
    ArtistUpdated res ->
      -- TODO : Error handling !!
      ( { model | page = ArtistListing }
      , getArtists)




getArtist : Int -> Effects.Effects Action
getArtist id =
  Http.get artist ("http://localhost:8081/artists/" ++ toString id)
    |> Task.toMaybe
    |> Task.map ArtistRetrieved
    |> Effects.task


getArtists : Effects.Effects Action
getArtists =
  Http.get artists "http://localhost:8081/artists"
    |> Task.toMaybe
    |> Task.map ArtistsRetrieved
    |> Effects.task


deleteArtist : Int -> Effects.Effects Action
deleteArtist id =
  Http.send Http.defaultSettings
        { verb = "DELETE"
        , url = "http://localhost:8081/artists/" ++ toString id
        , body = Http.empty
        , headers = []
        }
    |> Task.toResult
    |> Task.map ArtistDeleted
    |> Effects.task


updateArtist : Artist -> Effects.Effects Action
updateArtist a =
  Http.send Http.defaultSettings
        { verb = "PUT"
        , url = "http://localhost:8081/artists/" ++ toString a.id
        , body = Http.string (encodeArtist a)
        , headers = [("Content-Type", "application/json")]
        }
    |> Task.toResult
    |> Task.map ArtistUpdated
    |> Effects.task


encodeArtist : Artist -> String
encodeArtist a =
  JsonEncode.encode 0 <|
    JsonEncode.object
      [
        ("artistName", JsonEncode.string a.name)
      ]

artist : Json.Decoder Artist
artist =
  Json.object2 Artist
    ("artistId" := Json.int)
    ("artistName" := Json.string)


artists : Json.Decoder (List Artist)
artists =
  Json.list artist


-------- VIEW ---------

artistView : Signal.Address Action -> Model -> Html
artistView address model =
  div [] [
      h1 [] [text "Edit artist"]
    , Html.form [class "form-horizontal"] [
        div [class "form-group"] [
            label [class "col-sm-2 control-label"] [text "Name"]
          , div [class "col-sm-10"] [
              input [
                  class "form-control"
                , value model.currArtist.name
                , on "input" targetValue (\str -> Signal.message address (SetArtistName str))
              ] []
            ]
        ]
        , div [class "form-group"] [
            div [class "col-sm-offset-2 col-sm-10"] [
              button [
                  class "btn btn-default"
                , type' "button"
                , onClick address UpdateArtist ] [text "Save"]
            ]
        ]
    ]
  ]






artistRow : Signal.Address Action -> Artist -> Html
artistRow address artist =
  tr [] [
     td [] [text artist.name]
    ,td [] [button [ onClick address (GetArtist (.id artist))] [ text "Edit" ]]
    ,td [] [button [ onClick address (DeleteArtist (.id artist))] [ text "Delete!" ]]
  ]




artistsView : Signal.Address Action -> Model -> Html
artistsView address model =
  div [] [
      h1 [] [text "Artists" ]
    , table [class "table table-striped"] [
          thead [] [
            tr [] [
               th [] [text "Name"]
              ,th [] []
              ,th [] []
          ]
        ]
        , tbody [] (List.map (artistRow address) model.artists)
    ]
  ]

view : Signal.Address Action -> Model -> Html
view address model =
  div [class "container-fluid"] [
    case model.page of
      ArtistListing -> artistsView address model
      ArtistDetail -> artistView address model

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
