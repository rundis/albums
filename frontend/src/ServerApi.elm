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


getArtist : Int -> (Maybe Artist -> a) -> Effects.Effects a
getArtist id action =
    Http.get artistDecoder (baseUrl ++ "/artists/" ++ toString id)
        |> Task.toMaybe
        |> Task.map action
        |> Effects.task


getArtists : (Maybe (List Artist) -> a) -> Effects a
getArtists action =
    Http.get artistsDecoder (baseUrl ++ "/artists")
        |> Task.toMaybe
        |> Task.map action
        |> Effects.task


createArtist : ArtistRequest a -> (Maybe Artist -> b) -> Effects.Effects b
createArtist artist action =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl ++ "/artists"
        , body = Http.string (encodeArtist artist)
        , headers = [ ( "Content-Type", "application/json" ) ]
        }
        |> Http.fromJson artistDecoder
        |> Task.toMaybe
        |> Task.map action
        |> Effects.task


updateArtist : Artist -> (Maybe Artist -> a) -> Effects.Effects a
updateArtist artist action =
    Http.send Http.defaultSettings
        { verb = "PUT"
        , url = baseUrl ++ "/artists/" ++ toString artist.id
        , body = Http.string (encodeArtist artist)
        , headers = [ ( "Content-Type", "application/json" ) ]
        }
        |> Http.fromJson artistDecoder
        |> Task.toMaybe
        |> Task.map action
        |> Effects.task


deleteArtist : Int -> (Maybe Http.Response -> a) -> Effects.Effects a
deleteArtist id action =
    Http.send Http.defaultSettings
        { verb = "DELETE"
        , url = baseUrl ++ "/artists/" ++ toString id
        , body = Http.empty
        , headers = []
        }
        |> Task.toMaybe
        |> Task.map action
        |> Effects.task


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
    JsonE.encode 0
        <| JsonE.object
            [ ( "artistName", JsonE.string a.name )
            ]


getAlbum : Int -> (Maybe Album -> a) -> Effects.Effects a
getAlbum id action =
    Http.get albumDecoder (baseUrl ++ "/albums/" ++ toString id)
        |> Task.toMaybe
        |> Task.map action
        |> Effects.task


getAlbumsByArtist : Int -> (Maybe (List Album) -> a) -> Effects a
getAlbumsByArtist artistId action =
    Http.get albumsDecoder (baseUrl ++ "/albums?artistId=" ++ toString artistId)
        |> Task.toMaybe
        |> Task.map action
        |> Effects.task


updateAlbum : Album -> (Maybe Album -> a) -> Effects.Effects a
updateAlbum album action =
    Http.send Http.defaultSettings
        { verb = "PUT"
        , url = baseUrl ++ "/albums/" ++ toString album.id
        , body = Http.string (encodeAlbum album)
        , headers = [ ( "Content-Type", "application/json" ) ]
        }
        |> Http.fromJson albumDecoder
        |> Task.toMaybe
        |> Task.map action
        |> Effects.task


createAlbum : AlbumRequest a -> (Maybe Album -> b) -> Effects.Effects b
createAlbum album action =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl ++ "/albums"
        , body = Http.string (encodeAlbum album)
        , headers = [ ( "Content-Type", "application/json" ) ]
        }
        |> Http.fromJson albumDecoder
        |> Task.toMaybe
        |> Task.map action
        |> Effects.task


deleteAlbum : Int -> (Maybe Http.Response -> a) -> Effects.Effects a
deleteAlbum id action =
    Http.send Http.defaultSettings
        { verb = "DELETE"
        , url = baseUrl ++ "/albums/" ++ toString id
        , body = Http.empty
        , headers = []
        }
        |> Task.toMaybe
        |> Task.map action
        |> Effects.task


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
    JsonE.encode 0
        <| JsonE.object
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
