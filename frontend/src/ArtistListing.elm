module ArtistListing exposing (Model, Msg, init, view, update)

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
    | HandleArtistsRetrieved (List Artist)
    | FetchArtistsFailed Http.Error
    | DeleteArtist Int
    | HandleArtistDeleted
    | DeleteFailed


init : Model
init =
    Model [] []


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Show ->
            ( model, getArtists FetchArtistsFailed HandleArtistsRetrieved )

        HandleArtistsRetrieved artists ->
            ( { model | artists = artists }
            , Cmd.none
            )

        -- Handle error
        FetchArtistsFailed err ->
            ( model, Cmd.none )

        DeleteArtist id ->
            ( model, deleteArtist id DeleteFailed HandleArtistDeleted )

        HandleArtistDeleted ->
            update Show model

        -- Show generic error
        DeleteFailed ->
            ( model, Cmd.none )



------ VIEW ------


artistRow : Artist -> Html Msg
artistRow artist =
    tr []
        [ td [] [ text artist.name ]
        , td []
            [ a
                [ class "btn btn-sm btn-default"
                , href <| Routes.encode <| Routes.ArtistDetailPage artist.id
                ]
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


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Artists" ]
        , a
            [ class "pull-right btn btn-default"
            , href <| Routes.encode Routes.NewArtistPage
            ]
            [ text "New Artist" ]
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
