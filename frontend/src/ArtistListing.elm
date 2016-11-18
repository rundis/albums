module ArtistListing exposing (Model, Msg, init, view, update, mountCmd)

import ServerApi exposing (..)
import Routes
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Http


type alias Model =
    { artists : List Artist
    , errors : List String
    }


type Msg
    = Show
    | HandleArtistsRetrieved (Result Http.Error (List Artist))
    | DeleteArtist Int
    | HandleArtistDeleted (Result Http.Error String)


init : Model
init =
    Model [] []


mountCmd : Cmd Msg
mountCmd =
    ServerApi.getArtists HandleArtistsRetrieved


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Show ->
            ( model, mountCmd )

        HandleArtistsRetrieved res ->
            case res of
                Result.Ok artists ->
                    ( { model | artists = artists }
                    , Cmd.none
                    )

                Result.Err err ->
                    let _ = Debug.log "Error retrieving artist" err
                    in
                        (model, Cmd.none)


        DeleteArtist id ->
            ( model, deleteArtist id HandleArtistDeleted )

        HandleArtistDeleted res ->
            case res of
                Result.Ok _ ->
                    update Show model

                Result.Err err ->
                    let _ = Debug.log "Error deleting artist" err
                    in
                        (model, Cmd.none)





------ VIEW ------


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Artists" ]
        , Routes.linkTo Routes.NewArtistPage
            [ class "pull-right btn btn-default" ]
            [ i [ class "glyphicon glyphicon-plus" ] []
            , text " New Artist"
            ]
        , table [ class "table table-striped" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Name" ]
                    , th [] []
                    , th [] []
                    ]
                ]
            , tbody [] (List.map artistRow model.artists)
            ]
        ]


artistRow : Artist -> Html Msg
artistRow artist =
    tr []
        [ td [] [ text artist.name ]
        , td []
            [ Routes.linkTo (Routes.ArtistDetailPage artist.id)
                [ class "btn btn-sm btn-default" ]
                [ text "Edit" ]
            ]
        , td []
            [ button
                [ class "btn btn-sm btn-danger"
                , onClick <| DeleteArtist (.id artist)
                ]
                [ text "Delete!" ]
            ]
        ]
