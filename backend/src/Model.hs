{-# LANGUAGE DeriveGeneric #-}

module Model where

import GHC.Generics

data Artist = Artist
  { artistId :: Maybe Int
  , artistName :: String
  } deriving (Eq, Show, Generic)
