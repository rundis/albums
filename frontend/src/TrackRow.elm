module TrackRow (Model, init, Action, update, view, Context, Status, initPristine, isPristine) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)
import String

type alias Model =
  { name : String
  , durationMin : Maybe Int
  , durationSec : Maybe Int
  , status : Status
  }


init : String -> Maybe Int -> Model
init name duration =
  let
    (m, s) = duratationMinSecMaybes duration
  in
    Model name m s Saved

initPristine : Model
initPristine =
  Model "" Nothing Nothing Pristine


type alias Context =
  { actions : Signal.Address Action
  , remove : Signal.Address ()
  , moveUp : Signal.Address ()
  , moveDown : Signal.Address ()
  }


type Status
  = Saved
  | Modified
  | Error
  | Pristine


type Action
  = SetTrackName String
  | SetMinutes String
  | SetSeconds String





isPristine : Model -> Bool
isPristine model = model.status == Pristine



update : Action -> Model -> Model
update action model =
  case action of
    SetTrackName v ->
      { model | name = v, status = Modified }

    SetMinutes str ->
      let
        maybeMinutes = Result.toMaybe <| String.toInt str
      in
        case maybeMinutes of
          Just m ->
            { model | durationMin = maybeMinutes, status = Modified }

          Nothing ->
            if String.isEmpty str then
              { model | durationMin = Nothing, status = Modified}
            else
              model


    SetSeconds str ->
      let
        maybeSeconds = Result.toMaybe <| String.toInt str
      in
        case maybeSeconds of
          Just m ->
            if m < 60 then
              { model | durationSec = maybeSeconds, status = Modified }
            else
              model

          Nothing ->
            if String.isEmpty str then
              { model | durationSec = Nothing, status = Modified}
            else
              model


view : Context -> Model -> Html
view context model =
  tr
    []
    [ td [] [ statusView model ]
    , td [] [ moveView context model ]
    , td [] [ nameView context model ]
    , td [] [ durationView context model ]
    , td [] [ removeView context model ]
    ]

statusView : Model -> Html
statusView model =
  let
    clazz =
      case model.status of
        Saved ->
          "glyphicon-saved"

        Modified ->
          "glyphicon-pencil"

        Error ->
          "glyphicon-exclamation-sign"

        Pristine ->
          "glyphicon-asterisk"
  in
    span [ class ("glyphicon " ++ clazz) ] []


moveView : Context -> Model -> Html
moveView context model =
  div
    [class "btn-grp"]
    [moveUpView context model, moveDownView context model]


moveUpView : Context -> Model -> Html
moveUpView context model =
  button
    [ onClick context.moveUp ()
    , class <| "btn btn-sm btn-default " ++ if isPristine model then "disabled" else ""
    ]
    [ span [class "glyphicon glyphicon-arrow-up"]  []]


moveDownView : Context -> Model -> Html
moveDownView context model =
  button
    [ onClick context.moveDown ()
    , class <| "btn btn-sm btn-default " ++ if isPristine model then "disabled" else ""
    ]
    [ span [class "glyphicon glyphicon-arrow-down"]  []]


nameView : Context -> Model -> Html
nameView context model =
  input
    [ class "form-control"
    , value model.name
    , on "input" targetValue (\str -> Signal.message context.actions (SetTrackName str))
    ]
    []


durationView : Context -> Model -> Html
durationView context model =
  span
    []
        [ input
            [ class "form-control"
            , durStyle
            , maxlength 2
            , value  <| durvalToString model.durationMin
            , on "input" targetValue (\str -> Signal.message context.actions (SetMinutes str))
            ]
            []
        , span [style [("font-weight", "bold"), ("margin", "0 5px 0 5px")]] [text ":"]
        , input
            [ class "form-control"
            , durStyle
            , maxlength 2
            , value <| durvalToString model.durationSec
            , on "input" targetValue (\str -> Signal.message context.actions (SetSeconds str))
            ]
            []
        ]


durStyle : Attribute
durStyle =
  style [ ("width", "45px")
        , ("display", "inline-block")]


removeView : Context -> Model -> Html
removeView context model =
  button
    [ onClick context.remove ()
    , class <| "btn btn-sm btn-danger " ++ if isPristine model then "disabled" else ""
    ]
    [ text "Remove" ]

-- Duration view helper functions

duratationMinSecMaybes : Maybe Int -> (Maybe Int, Maybe Int)
duratationMinSecMaybes d =
  case d of
    Nothing -> (Nothing, Nothing)
    Just x -> let
                (m, s) = durationMinSec x
              in
                (Just m, Just s)


durationMinSec : Int -> (Int, Int)
durationMinSec d =
  (d // 60, rem d 60)


durvalToString : Maybe Int -> String
durvalToString durval =
  case durval of
    Nothing -> ""
    Just x -> toString x
