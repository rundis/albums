module ServerApi exposing (..)

import Json.Decode as JsonD exposing ((:=))
import Json.Encode as JsonE
import Http
import Task


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


getArtist : Int -> (Http.Error -> msg) -> (Artist -> msg) -> Cmd msg
getArtist id errorMsg msg =
    Http.get artistDecoder (baseUrl ++ "/artists/" ++ toString id)
        |> Task.perform errorMsg msg


getArtists : (Http.Error -> msg) -> (List Artist -> msg) -> Cmd msg
getArtists errorMsg msg =
    Http.get artistsDecoder (baseUrl ++ "/artists")
        |> Task.perform errorMsg msg


createArtist : ArtistRequest a -> (Http.Error -> msg) -> (Artist -> msg) -> Cmd msg
createArtist artist errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl ++ "/artists"
        , body = Http.string (encodeArtist artist)
        , headers = [ ( "Content-Type", "application/json" ) ]
        }
        |> Http.fromJson artistDecoder
        |> Task.perform errorMsg msg


updateArtist : Artist -> (Http.Error -> msg) -> (Artist -> msg) -> Cmd msg
updateArtist artist errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "PUT"
        , url = baseUrl ++ "/artists/" ++ toString artist.id
        , body = Http.string (encodeArtist artist)
        , headers = [ ( "Content-Type", "application/json" ) ]
        }
        |> Http.fromJson artistDecoder
        |> Task.perform errorMsg msg


deleteArtist : Int -> msg -> msg -> Cmd msg
deleteArtist id errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "DELETE"
        , url = baseUrl ++ "/artists/" ++ toString id
        , body = Http.empty
        , headers = []
        }
        |> Task.perform (\_ -> errorMsg) (\_ -> msg)


artistsDecoder : JsonD.Decoder (List Artist)
artistsDecoder =
    JsonD.list artistDecoder


artistDecoder : JsonD.Decoder Artist
artistDecoder =
    JsonD.object2 Artist
        ("artistId" := JsonD.int)
        ("artistName" := JsonD.string)


encodeArtist : ArtistRequest a -> String
encodeArtist a =
    JsonE.encode 0 <|
        JsonE.object
            [ ( "artistName", JsonE.string a.name )
            ]


getAlbum : Int -> (Http.Error -> msg) -> (Album -> msg) -> Cmd msg
getAlbum id errorMsg msg =
    Http.get albumDecoder (baseUrl ++ "/albums/" ++ toString id)
        |> Task.perform errorMsg msg


getAlbumsByArtist : Int -> (Http.Error -> msg) -> (List Album -> msg) -> Cmd msg
getAlbumsByArtist artistId errorMsg msg =
    Http.get albumsDecoder (baseUrl ++ "/albums?artistId=" ++ toString artistId)
        |> Task.perform errorMsg msg


updateAlbum : Album -> (Http.Error -> msg) -> (Album -> msg) -> Cmd msg
updateAlbum album errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "PUT"
        , url = baseUrl ++ "/albums/" ++ toString album.id
        , body = Http.string (encodeAlbum album)
        , headers = [ ( "Content-Type", "application/json" ) ]
        }
        |> Http.fromJson albumDecoder
        |> Task.perform errorMsg msg


createAlbum : AlbumRequest a -> (Http.Error -> msg) -> (Album -> msg) -> Cmd msg
createAlbum album errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl ++ "/albums"
        , body = Http.string (encodeAlbum album)
        , headers = [ ( "Content-Type", "application/json" ) ]
        }
        |> Http.fromJson albumDecoder
        |> Task.perform errorMsg msg


deleteAlbum : Int -> msg -> msg -> Cmd msg
deleteAlbum id errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "DELETE"
        , url = baseUrl ++ "/albums/" ++ toString id
        , body = Http.empty
        , headers = []
        }
        |> Task.perform (\_ -> errorMsg) (\_ -> msg)


albumsDecoder : JsonD.Decoder (List Album)
albumsDecoder =
    JsonD.list albumDecoder


albumDecoder : JsonD.Decoder Album
albumDecoder =
    JsonD.object4 Album
        ("albumId" := JsonD.int)
        ("albumName" := JsonD.string)
        ("albumArtistId" := JsonD.int)
        ("albumTracks" := JsonD.list trackDecoder)


trackDecoder : JsonD.Decoder Track
trackDecoder =
    JsonD.object2 Track
        ("trackName" := JsonD.string)
        ("trackDuration" := JsonD.int)


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
