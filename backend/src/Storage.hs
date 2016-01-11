{-# LANGUAGE OverloadedStrings #-}
module Storage where


import qualified Model as M
import qualified Data.Text as Txt


import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.Types as SqlTypes


instance Sql.FromRow M.Artist where
  fromRow = M.Artist <$> Sql.field <*> Sql.field



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
  return artist
  where
    params = [":id" := (idParam :: Int), ":name" := ((M.artistName artist) :: String)]


deleteArtist :: Sql.Connection -> Int -> IO ()
deleteArtist conn idParam =
  Sql.execute conn "delete from artist where id = ?" (Sql.Only idParam)


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


bootstrapDB :: Sql.Connection -> IO ()
bootstrapDB conn = do
  executeDB "create table artist (id integer primary key asc, name varchar2(255))"
  mapM_ insertArtist testArtists

  where
    testArtists = ["Metallica", "Megadeth", "Pantera", "Machine Head", "Crowbar", "Sepultura", "Rage against the machine"]
    insertArtist name = Sql.execute conn "insert into artist (name) values (?)" $ Sql.Only (name :: String)
    executeDB = Sql.execute_ conn
