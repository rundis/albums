module ServerApi exposing (..)

import Json.Decode as JsonD
import Json.Encode as JsonE
import Http


type alias ArtistRequest a =
    { a | name : String }


type alias Artist =
    { id : Int
    , name : String
    }


type alias AlbumRequest a =
    { a
        | name : String
        , artistId : Int
        , tracks : List Track
    }


type alias Album =
    { id : Int
    , name : String
    , artistId : Int
    , tracks : List Track
    }


type alias Track =
    { name : String
    , duration : Int
    }


baseUrl : String
baseUrl =
    "http://localhost:8081"



getArtist : Int -> (Result Http.Error Artist -> msg) -> Cmd msg
getArtist id msg =
    Http.get (baseUrl ++ "/artists/" ++ toString id) artistDecoder
        |> Http.send msg



getArtists : (Result Http.Error (List Artist) -> msg) -> Cmd msg
getArtists msg =
    Http.get (baseUrl ++ "/artists") artistsDecoder
        |> Http.send msg




createArtist : ArtistRequest a -> (Result Http.Error Artist -> msg) -> Cmd msg
createArtist artist msg =
    Http.post
        (baseUrl ++ "/artists")
        (Http.stringBody "application/json" <| encodeArtist artist)
        artistDecoder
        |> Http.send msg




updateArtist: Artist -> (Result Http.Error Artist -> msg) -> Cmd msg
updateArtist artist msg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = baseUrl ++ "/artists/" ++ toString artist.id
        , body = Http.stringBody "application/json" <| encodeArtist artist
        , expect = Http.expectJson artistDecoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg




deleteArtist : Int -> (Result Http.Error String -> msg) -> Cmd msg
deleteArtist id  msg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = baseUrl ++ "/artists/" ++ toString id
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg


artistsDecoder : JsonD.Decoder (List Artist)
artistsDecoder =
    JsonD.list artistDecoder


artistDecoder : JsonD.Decoder Artist
artistDecoder =
    JsonD.map2 Artist
        (JsonD.field "artistId" JsonD.int)
        (JsonD.field "artistName" JsonD.string)


encodeArtist : ArtistRequest a -> String
encodeArtist a =
    JsonE.encode 0 <|
        JsonE.object
            [ ( "artistName", JsonE.string a.name )
            ]


getAlbum : Int -> (Result Http.Error Album -> msg) -> Cmd msg
getAlbum id msg =
    Http.get (baseUrl ++ "/albums/" ++ toString id) albumDecoder
        |> Http.send msg


getAlbumsByArtist : Int -> (Result Http.Error (List Album) -> msg) -> Cmd msg
getAlbumsByArtist artistId msg =
    Http.get (baseUrl ++ "/albums?artistId=" ++ toString artistId) albumsDecoder
        |> Http.send msg



createAlbum : AlbumRequest a -> (Result Http.Error Album -> msg) -> Cmd msg
createAlbum album msg =
    Http.post
        (baseUrl ++ "/albums")
        (Http.stringBody "application/json" <| encodeAlbum album)
        albumDecoder
        |> Http.send msg

updateAlbum: Album -> (Result Http.Error Album -> msg) -> Cmd msg
updateAlbum album msg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = baseUrl ++ "/albums/" ++ toString album.id
        , body = Http.stringBody "application/json" <| encodeAlbum album
        , expect = Http.expectJson albumDecoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg

deleteAlbum : Int -> (Result Http.Error String -> msg) -> Cmd msg
deleteAlbum id  msg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = baseUrl ++ "/albums/" ++ toString id
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg


albumsDecoder : JsonD.Decoder (List Album)
albumsDecoder =
    JsonD.list albumDecoder


albumDecoder : JsonD.Decoder Album
albumDecoder =
    JsonD.map4 Album
        (JsonD.field "albumId"  JsonD.int)
        (JsonD.field "albumName"  JsonD.string)
        (JsonD.field "albumArtistId"  JsonD.int)
        (JsonD.field "albumTracks" <| JsonD.list trackDecoder)


trackDecoder : JsonD.Decoder Track
trackDecoder =
    JsonD.map2 Track
        (JsonD.field "trackName" JsonD.string)
        (JsonD.field "trackDuration" JsonD.int)


encodeAlbum : AlbumRequest a -> String
encodeAlbum album =
    JsonE.encode 0 <|
        JsonE.object
            [ ( "albumName", JsonE.string album.name )
            , ( "albumArtistId", JsonE.int album.artistId )
            , ( "albumTracks", JsonE.list <| List.map encodeTrack album.tracks )
            ]


encodeTrack : Track -> JsonE.Value
encodeTrack track =
    JsonE.object
        [ ( "trackName", JsonE.string track.name )
        , ( "trackDuration", JsonE.int track.duration )
        ]
