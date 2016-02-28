{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Bootstrap where

import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.Types as SqlTypes


bootstrapDB :: Sql.Connection -> IO ()
bootstrapDB conn = do
  createSchema conn
  populateSampleData conn


createSchema :: Sql.Connection -> IO ()
createSchema conn = do
  executeDB "PRAGMA foreign_keys = ON"
  executeDB "create table artist (id integer primary key asc, name varchar2(255))"
  executeDB "create table track (id integer primary key asc, name varchar2(255), duration integer)"
  executeDB "create table album (id integer primary key asc, artist_id integer, name varchar2(255), FOREIGN KEY(artist_id) references artist(id))"
  executeDB "create table album_track (track_no integer, album_id, track_id, primary key(track_no, album_id, track_id), foreign key(album_id) references album(id), foreign key(track_id) references track(id))"

  where
    executeDB = Sql.execute_ conn

artists :: [(Int, String)]
artists = [(1, "Metallica"), (2, "Megadeth"), (3, "Pantera"), (4, "Machine Head"), (5, "Crowbar"), (6, "Sepultura"), (7, "Rage against the machine")]

tracks :: [(Int, String, Int)]
tracks = [ (1, "Enter Sandman", 332)
         , (2, "Sad but true", 325)
         , (3, "Holier than thou", 228)

         , (100, "Fight fire with fire", 285)
         , (101, "Ride the Lightning", 338)
         , (102, "For whom the bell tolls", 312)
         , (103, "Fade to black", 414)
         ]


albums :: [(Int, Int, String)]
albums = [ (1, 1, "Black")
         , (2, 1, "Ride the Lightning")
         ]


albumTracks :: [(Int, Int, Int)]
albumTracks = [(1, 1, 1), (2, 1, 2), (3, 1, 3), -- Black album tracks
               (1, 2, 100), (2, 2, 101), (3, 2, 102), (4, 2, 103) --Ride the Lightning
              ]


populateSampleData :: Sql.Connection -> IO ()
populateSampleData conn = do
  mapM_ insertArtist artists
  mapM_ insertTrack tracks
  mapM_ insertAlbum albums
  mapM_ insertAlbumTrack albumTracks

  where
    insertArtist a = Sql.execute conn "insert into artist (id, name) values (?, ?)" a
    insertTrack t = Sql.execute conn "insert into track (id, name, duration) values (?, ?, ?)" t
    insertAlbum a = Sql.execute conn "insert into album (id, artist_id, name) values (?, ?, ?)" a
    insertAlbumTrack at = Sql.execute conn "insert into album_track (track_no, album_id, track_id) values (?, ?, ?)" at
