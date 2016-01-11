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


liftIOMaybeToEither ::  (MonadIO m) => a -> IO (Maybe b) -> EitherT a m b
liftIOMaybeToEither err x = do
    m <- liftIO x
    case m of
      Nothing -> left err
      Just x -> right x



type API = "artists" :> ArtistAPI


api :: Proxy API
api = Proxy
