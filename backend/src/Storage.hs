{-# LANGUAGE OverloadedStrings #-}
module Storage where


import qualified Model as M
import qualified Data.Text as Txt
import qualified Data.Map.Strict as Map
import Control.Monad
import Data.Maybe


import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.Types as SqlTypes


instance Sql.FromRow M.Artist where
  fromRow = M.Artist <$> Sql.field <*> Sql.field

instance Sql.FromRow M.Track where
  fromRow = M.Track <$> Sql.field <*> Sql.field <*> Sql.field



artistById :: Sql.Connection -> Int -> IO (Maybe M.Artist)
artistById conn idParam =
  findById conn "artist" idParam :: IO (Maybe M.Artist)


findArtists :: Sql.Connection -> IO [M.Artist]
findArtists conn =
  Sql.query_ conn "select * from artist" :: IO [M.Artist]


newArtist :: Sql.Connection -> M.Artist -> IO M.Artist
newArtist conn artist = do
  Sql.execute conn "insert into artist (name) values (?) " (Sql.Only $ M.artistName artist)
  rawId <- lastInsertRowId conn
  let updArtist = artist { M.artistId = Just (fromIntegral rawId) }
  return updArtist


-- Really we should check whether the artist exists here
updateArtist :: Sql.Connection -> M.Artist -> Int -> IO M.Artist
updateArtist conn artist idParam = do
  Sql.executeNamed conn "update artist set name = :name where id = :id" params
  return artist { M.artistId = Just idParam }
  where
    params = [":id" := (idParam :: Int), ":name" := ((M.artistName artist) :: String)]


deleteArtist :: Sql.Connection -> Int -> IO ()
deleteArtist conn artistId = do
  albums <- findAlbumsByArtist conn artistId
  mapM_ (\a -> deleteAlbum conn $ fromMaybe (-1) (M.albumId a)  ) albums
  Sql.execute conn "delete from artist where id = ?" (Sql.Only artistId)




findAlbums :: Sql.Connection -> IO [M.Album]
findAlbums conn = do
  rows <- Sql.query_ conn (albumsQuery "") :: IO [(Int, String, Int, Int, String, Int)]
  return $ Map.elems $ foldl groupAlbum Map.empty rows


findAlbumsByArtist :: Sql.Connection -> Int -> IO [M.Album]
findAlbumsByArtist conn artistId = do
  rows <- Sql.query conn (albumsQuery " where artist_id = ?") (Sql.Only artistId) :: IO [(Int, String, Int, Int, String, Int)]
  return $ Map.elems $ foldl groupAlbum Map.empty rows


albumsQuery :: String -> SqlTypes.Query
albumsQuery whereClause =
  SqlTypes.Query $ Txt.pack $
    "select a.id, a.name, a.artist_id, t.id, t.name, t.duration \
    \ from album a inner join album_track at on a.id = at.album_id \
    \ inner join track t on at.track_id = t.id"
    ++ whereClause
    ++ " order by a.id, at.track_no"


groupAlbum :: Map.Map Int M.Album -> (Int, String, Int, Int, String, Int) -> Map.Map Int M.Album
groupAlbum acc (albumId, albumName, artistId, trackId, trackName, trackDuration) =
  case (Map.lookup albumId acc) of
    Nothing -> Map.insert albumId (M.Album (Just albumId) albumName artistId [M.Track (Just trackId) trackName trackDuration]) acc
    Just _ -> Map.update (\a -> Just (addTrack a (trackId, trackName, trackDuration))) albumId acc
              where
                addTrack album (trackId, trackName, trackDuration) =
                  album {M.albumTracks = (M.albumTracks album) ++ [M.Track (Just trackId) trackName trackDuration]}


albumById :: Sql.Connection -> Int -> IO (Maybe M.Album)
albumById conn albumId = do
  r <- (Sql.query conn "select name, artist_id from album where id = ?" (Sql.Only albumId) :: IO [(String, Int)])
  case (length r) of
    0 -> return Nothing
    _ -> let
           (name, artistId) = head r
         in
           do
             tracks <- (tracksByAlbumId conn albumId)
             return $ Just $ M.Album (Just albumId) name artistId tracks



tracksByAlbumId :: Sql.Connection -> Int -> IO [M.Track]
tracksByAlbumId conn albumId = do
  Sql.query conn "select t.id, t.name, t.duration from track t inner join album_track at on t.id = at.track_id where at.album_id = ? order by at.track_no" (Sql.Only albumId)


newAlbum :: Sql.Connection -> M.Album -> IO M.Album
newAlbum conn album = do
  Sql.executeNamed conn "insert into album (name, artist_id) values (:name, :artistId)" [":name" := (M.albumName album), ":artistId" := (M.albumArtistId album)]
  albumId <- lastInsertRowId conn
  tracks <- zipWithM (\t i -> newTrack conn (i, fromIntegral albumId, (M.albumArtistId album), t)) (M.albumTracks album) [0..]

  return album { M.albumId = Just $ fromIntegral albumId
               , M.albumTracks = tracks
               }


updateAlbum :: Sql.Connection -> M.Album -> Int -> IO (Maybe M.Album)
updateAlbum conn album albumId = do
  x <- albumById conn albumId
  case x of
    Nothing -> return x

    Just curr -> do
      mapM_ delTrack $ M.albumTracks curr
      Sql.executeNamed conn updSql updParams
      tracks <- zipWithM newTrack'  (M.albumTracks album) [0..]

      return (Just curr { M.albumName = M.albumName album
                        , M.albumArtistId = M.albumArtistId album
                        , M.albumTracks = tracks
                        })

      where
        delTrack t = deleteAlbumTrack conn (fromJust (M.trackId t))
        updSql = "update album set name = :name, artist_id = :artistId where id = :id"
        updParams = [ ":name" := (M.albumName album)
                    , ":artistId" := (M.albumArtistId album)
                    , ":id" := albumId
                    ]
        newTrack' t idx = newTrack conn (idx, albumId, M.albumArtistId album, t)



deleteAlbum :: Sql.Connection -> Int -> IO ()
deleteAlbum conn albumId = do
  x <- albumById conn albumId
  case x of
    Nothing -> return ()

    Just curr -> do
      mapM_ delTrack $ M.albumTracks curr
      Sql.execute conn "delete from album where id=?" (Sql.Only albumId)

      return ()

      where
        delTrack t = deleteAlbumTrack conn (fromJust (M.trackId t))


newTrack :: Sql.Connection -> (Int, Int, Int, M.Track) -> IO M.Track
newTrack conn (trackNo, albumId, artistId, track) = do
  Sql.executeNamed conn "insert into track (name, duration) values (:name, :duration)" [":name" := (M.trackName track), ":duration" := (M.trackDuration track)]
  trackId <- lastInsertRowId conn
  Sql.execute conn "insert into album_track (track_no, album_id, track_id) values (?, ?, ?)" (trackNo, albumId, trackId)

  return track {M.trackId = Just $ fromIntegral trackId}


deleteAlbumTrack :: Sql.Connection -> Int -> IO ()
deleteAlbumTrack conn trackId = do
  Sql.execute conn "delete from album_track where track_id = ?" (Sql.Only trackId)
  Sql.execute conn "delete from track where id = ?" (Sql.Only trackId)


findById :: (FromRow a) => Sql.Connection -> String -> Int -> IO (Maybe a)
findById conn table idParam = do
  rows <- Sql.queryNamed conn (createFindByIdQuery table) [":id" := (idParam :: Int)]
  let result = case (length rows) of
                  0 -> Nothing
                  _ -> Just $ head rows

  return result


createFindByIdQuery :: String -> SqlTypes.Query
createFindByIdQuery table =
  SqlTypes.Query $ Txt.pack $ "SELECT * from " ++ table ++ " where id = :id"

