{-# LANGUAGE OverloadedStrings #-}
module Main where


import qualified Api as A
import qualified Bootstrap as B
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Middleware.Cors
import Control.Exception (bracket)
import Database.SQLite.Simple as Sql


app :: Sql.Connection -> Application
app conn = serve A.api (A.combinedServer conn)


testConnect :: IO Sql.Connection
testConnect = Sql.open ":memory:"


withTestConnection :: (Sql.Connection -> IO a) -> IO a
withTestConnection cb =
  withConn $ \conn -> cb conn
  where
    withConn = bracket testConnect Sql.close


albumCors :: Middleware
albumCors = cors $ const (Just albumResourcePolicy)


albumResourcePolicy :: CorsResourcePolicy
albumResourcePolicy =
    CorsResourcePolicy
        { corsOrigins = Nothing -- gives you /*
        , corsMethods = ["GET", "POST", "PUT", "DELETE", "HEAD", "OPTION"]
        , corsRequestHeaders = simpleHeaders -- adds "Content-Type" to defaults
        , corsExposedHeaders = Nothing
        , corsMaxAge = Nothing
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }



main :: IO ()
main = do
  withTestConnection $ \conn ->  do
    B.bootstrapDB conn
    run 8081 $ albumCors $ app conn
