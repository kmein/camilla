{-# LANGUAGE OverloadedStrings #-}
module Main where

import Camilla.HTTP
import Camilla.Types
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.Monoid ((<>))
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
import Options.Applicative

confParser :: Parser HTTPConfig
confParser =
    HTTPConfig <$>
    option
        auto
        (long "ip" <> short 'i' <> metavar "ADDR" <> help "CMI IP address") <*>
    strOption
        (long "user" <> short 'u' <> metavar "NAME" <> help "CMI username" <> value "admin") <*>
    strOption
        (long "pass" <> short 'p' <> metavar "PASS" <> help "CMI password" <> value "admin")

dbPathParser :: Parser FilePath
dbPathParser =
    strOption
        (long "database" <> short 'd' <> metavar "PATH" <>
         help "output database path")

-- reliable interval: 24.5 s
readWithInterval :: Double -> HTTPConfig -> Request -> (Response -> IO a) -> IO ()
readWithInterval secs conf req f = forever $ do
    f =<< readCMI conf req
    threadDelay $ truncate $ secs * 1000000

createTable :: (IConnection conn) => conn -> IO Integer
createTable conn =
    run
        conn
        "CREATE TABLE cmi (id ROWID, version TEXT NOT NULL, device TEXT NOT NULL, timestamp INTEGER, jsonparam TEXT NOT NULL, number INTEGER, value REAL, unit TEXT NOT NULL)"
        []

main :: IO ()
main = do
    (conf, dbPath) <- execParser opts
    bracket (connectSqlite3 dbPath) disconnect $ \conn -> do
        createTable conn
        insertRow <-
            prepare
                conn
                "INSERT INTO cmi (version,device,timestamp,jsonparam,number,value,unit) VALUES (?,?,?,?,?,?,?)"
        readWithInterval 24.5 conf req $ \resp -> do
            print resp
            executeMany insertRow $ toSqlRows resp
            commit conn
  where
    req = Request 1 [JSONParam Inputs Nothing, JSONParam Outputs Nothing]
    opts = info (((,) <$> confParser <*> dbPathParser) <**> helper) fullDesc
