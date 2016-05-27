module TrackRow exposing (Model, init, Msg, DispatchMsg(..), update, view, Status, initPristine, isPristine)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
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
        ( m, s ) =
            duratationMinSecMaybes duration
    in
        Model name m s Saved


initPristine : Model
initPristine =
    Model "" Nothing Nothing Pristine


type Status
    = Saved
    | Modified
    | Error
    | Pristine


type Msg
    = SetTrackName String
    | SetMinutes String
    | SetSeconds String
    | Dispatch DispatchMsg


type DispatchMsg
    = MoveUp
    | MoveDown
    | Remove


isPristine : Model -> Bool
isPristine model =
    model.status == Pristine


update : Msg -> Model -> ( Model, Maybe DispatchMsg )
update action model =
    case action of
        SetTrackName v ->
            ( { model | name = v, status = Modified }, Nothing )

        SetMinutes str ->
            let
                maybeMinutes =
                    Result.toMaybe <| String.toInt str
            in
                case maybeMinutes of
                    Just m ->
                        ( { model | durationMin = maybeMinutes, status = Modified }, Nothing )

                    Nothing ->
                        if String.isEmpty str then
                            ( { model | durationMin = Nothing, status = Modified }, Nothing )
                        else
                            ( model, Nothing )

        SetSeconds str ->
            let
                maybeSeconds =
                    Result.toMaybe <| String.toInt str
            in
                case maybeSeconds of
                    Just m ->
                        if m < 60 then
                            ( { model | durationSec = maybeSeconds, status = Modified }, Nothing )
                        else
                            ( model, Nothing )

                    Nothing ->
                        if String.isEmpty str then
                            ( { model | durationSec = Nothing, status = Modified }, Nothing )
                        else
                            ( model, Nothing )

        Dispatch dispatchMsg ->
            ( model, Just dispatchMsg )


view : Model -> Html Msg
view model =
    tr []
        [ td [] [ statusView model ]
        , td [] [ moveView model ]
        , td [] [ nameView model ]
        , td [] [ durationView model ]
        , td [] [ removeView model ]
        ]


statusView : Model -> Html Msg
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


moveView : Model -> Html Msg
moveView model =
    div [ class "btn-grp" ]
        [ moveUpView model, moveDownView model ]


moveUpView : Model -> Html Msg
moveUpView model =
    button
        [ onClick (Dispatch MoveUp)
        , class
            <| "btn btn-sm btn-default "
            ++ if isPristine model then
                "disabled"
               else
                ""
        ]
        [ span [ class "glyphicon glyphicon-arrow-up" ] [] ]


moveDownView : Model -> Html Msg
moveDownView model =
    button
        [ onClick (Dispatch MoveDown)
        , class
            <| "btn btn-sm btn-default "
            ++ if isPristine model then
                "disabled"
               else
                ""
        ]
        [ span [ class "glyphicon glyphicon-arrow-down" ] [] ]


nameView : Model -> Html Msg
nameView model =
    input
        [ class "form-control"
        , value model.name
        , onInput SetTrackName
        ]
        []


durationView : Model -> Html Msg
durationView model =
    span []
        [ input
            [ class "form-control"
            , durStyle
            , maxlength 2
            , value <| durvalToString model.durationMin
            , onInput SetMinutes
            ]
            []
        , span [ style [ ( "font-weight", "bold" ), ( "margin", "0 5px 0 5px" ) ] ] [ text ":" ]
        , input
            [ class "form-control"
            , durStyle
            , maxlength 2
            , value <| durvalToString model.durationSec
            , onInput SetSeconds
            ]
            []
        ]


durStyle : Attribute Msg
durStyle =
    style
        [ ( "width", "45px" )
        , ( "display", "inline-block" )
        ]


removeView : Model -> Html Msg
removeView model =
    button
        [ onClick (Dispatch Remove)
        , class
            <| "btn btn-sm btn-danger "
            ++ if isPristine model then
                "disabled"
               else
                ""
        ]
        [ text "Remove" ]



-- Duration view helper functions


duratationMinSecMaybes : Maybe Int -> ( Maybe Int, Maybe Int )
duratationMinSecMaybes d =
    case d of
        Nothing ->
            ( Nothing, Nothing )

        Just x ->
            let
                ( m, s ) =
                    durationMinSec x
            in
                ( Just m, Just s )


durationMinSec : Int -> ( Int, Int )
durationMinSec d =
    ( d // 60, rem d 60 )


durvalToString : Maybe Int -> String
durvalToString durval =
    case durval of
        Nothing ->
            ""

        Just x ->
            toString x
