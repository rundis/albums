{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module Api where

import qualified Model as M
import qualified Storage as S
import Data.Aeson
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Either
import Servant
import Database.SQLite.Simple as Sql

instance ToJSON M.Artist
instance FromJSON M.Artist
instance ToJSON M.Track
instance FromJSON M.Track
instance ToJSON M.Album
instance FromJSON M.Album



type ArtistAPI =
       Get '[JSON] [M.Artist]
  :<|> ReqBody '[JSON] M.Artist :> Post '[JSON] M.Artist
  :<|> Capture "artistId" Int :> Get '[JSON] M.Artist
  :<|> Capture "artistId" Int :> ReqBody '[JSON] M.Artist :> Put '[JSON] M.Artist
  :<|> Capture "artistId" Int :> Delete '[] ()



artistsServer :: Sql.Connection -> Server ArtistAPI
artistsServer conn =
  getArtists :<|> postArtist :<|> getArtist :<|>  updateArtist :<|> deleteArtist

  where
    getArtists                   = liftIO $ S.findArtists conn
    getArtist artistId           = liftIOMaybeToEither err404 $ S.artistById conn artistId
    postArtist artist            = liftIO $ S.newArtist conn artist
    updateArtist artistId artist = liftIO $ S.updateArtist conn artist artistId
    deleteArtist artistId        = liftIO $ S.deleteArtist conn artistId


type AlbumAPI =
       QueryParam "artistId" Int :> Get '[JSON] [M.Album]
  :<|> ReqBody '[JSON] M.Album :> Post '[JSON] M.Album
  :<|> Capture "albumId" Int :> ReqBody '[JSON] M.Album :> Put '[JSON] M.Album
  :<|> Capture "albumId" Int :> Get '[JSON] M.Album
  :<|> Capture "albumId" Int :> Delete '[] ()


albumsServer :: Sql.Connection -> Server AlbumAPI
albumsServer conn =
  getAlbums :<|> postAlbum :<|> updateAlbum :<|> getAlbum :<|> deleteAlbum

  where
    getAlbums artistId            = liftIO $ case artistId of
                                              Nothing -> S.findAlbums conn
                                              Just x -> S.findAlbumsByArtist conn x
    postAlbum album               = liftIO $ Sql.withTransaction conn $ S.newAlbum conn album
    updateAlbum albumId album     = liftIOMaybeToEither err404 $ Sql.withTransaction conn $ S.updateAlbum conn album albumId
    getAlbum albumId              = liftIOMaybeToEither err404 $ S.albumById conn albumId
    deleteAlbum albumId           = liftIO $ Sql.withTransaction conn $ S.deleteAlbum conn albumId



liftIOMaybeToEither ::  (MonadIO m) => a -> IO (Maybe b) -> EitherT a m b
liftIOMaybeToEither err x = do
    m <- liftIO x
    case m of
      Nothing -> left err
      Just x -> right x



type API = "artists" :> ArtistAPI :<|> "albums" :> AlbumAPI

combinedServer :: Sql.Connection -> Server API
combinedServer conn = artistsServer conn :<|> albumsServer conn



api :: Proxy API
api = Proxy
