{-# LANGUAGE OverloadedStrings #-}
module Main where


import qualified Storage as S
import qualified Api as A
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Middleware.Cors
import Control.Exception (bracket)
import Database.SQLite.Simple as Sql


app :: Sql.Connection -> Application
app conn = serve A.api (A.artistsServer conn)


testConnect :: IO Sql.Connection
testConnect = Sql.open ":memory:"


withTestConnection :: (Sql.Connection -> IO a) -> IO a
withTestConnection cb =
  withConn $ \conn -> cb conn
  where
    withConn = bracket testConnect Sql.close


main :: IO ()
main = do
  withTestConnection $ \conn ->  do
    S.bootstrapDB conn
    run 8081 $ simpleCors $ (app conn)
