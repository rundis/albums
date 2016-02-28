{-# LANGUAGE DeriveGeneric #-}

module Model where

import GHC.Generics

data Artist = Artist
  { artistId :: Maybe Int
  , artistName :: String
  } deriving (Eq, Show, Generic)


data Track = Track
  { trackId :: Maybe Int
  , trackName :: String
  , trackDuration :: Int -- seconds
  } deriving (Eq, Show, Generic)


data Album = Album
  { albumId :: Maybe Int
  , albumName :: String
  , albumArtistId :: Int
  , albumTracks :: [Track]
  } deriving (Eq, Show, Generic)


