{-# LANGUAGE CPP           #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import Data.Aeson
import GHC.Generics
import Data.List
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Trans.Either
import Network.Wai.Middleware.Cors


data Artist = Artist
  { artistId :: Int
  , name :: String
  } deriving (Eq, Show, Generic)


instance ToJSON Artist

type ArtistAPI =
       Get '[JSON] [Artist]
  :<|> Capture "artistId" Int :> Get '[JSON] Artist


artistsServer :: Server ArtistAPI
artistsServer = getArtists :<|> artistOperations

  where getArtists :: EitherT ServantErr IO [Artist]
        getArtists = return artists

        artistOperations artistId =
          viewArtist

          where viewArtist :: EitherT ServantErr IO Artist
                viewArtist = artistById artistId



artistById :: Int -> EitherT ServantErr IO Artist
artistById idParam =
  case a of
    Nothing -> left (err404 {errBody = "No artist with given id exists"})
    Just b -> return b
  where
    a = find ((== idParam) . artistId) artists


artists :: [Artist]
artists =
  [ Artist 1 "Metallica"
  , Artist 2 "Megadeth"
  , Artist 3 "Pantera"
  , Artist 4 "Machine Head"
  , Artist 5 "Crowbar"
  , Artist 6 "Sepultura"
  , Artist 7 "Rage against the machine"
  ]



type API = "artists" :> ArtistAPI


api :: Proxy API
api = Proxy


app :: Application
app = serve api artistsServer


main :: IO ()
main = do run 8081 $ simpleCors $ app
