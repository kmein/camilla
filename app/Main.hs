{-# LANGUAGE OverloadedStrings #-}
module Main where

import Camilla.HTTP
import Camilla.Types
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever, unless, void)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
import Options.Applicative
import qualified Network.Wreq.Session as S

data Action = Write (Maybe String) String (Either Int Double) | ReadTo FilePath

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

actionParser :: Parser Action
actionParser =
    subparser
        (command "read" (info readToParser (progDesc "Read from CMI")) <>
         command "write" (info writeParser (progDesc "Write to CMI")))
  where
    writeParser =
        Write <$>
        optional (strOption (long "expert-password" <> metavar "PASS" <> value "128")) <*>
        strArgument (metavar "ADDR") <*>
        argument auto (metavar "X")
    readToParser =
        ReadTo <$>
        strOption
            (long "database" <> short 'd' <> metavar "PATH" <>
             help "output database path")

-- reliable interval: 24.5 s
readWithInterval :: Double -> HTTPConfig -> Request -> (Response -> IO a) -> IO ()
readWithInterval secs conf req f = forever $ do
    f =<< readCMI conf req
    threadDelay $ truncate $ secs * 1000000

createTable :: (IConnection conn) => conn -> IO ()
createTable conn =
    void $
    run
        conn
        "CREATE TABLE cmi (id ROWID, version TEXT NOT NULL, device TEXT NOT NULL, timestamp INTEGER, jsonparam TEXT NOT NULL, number INTEGER, value REAL, unit TEXT NOT NULL)"
        []

main :: IO ()
main = do
    (conf, action) <- execParser opts
    case action of
        Write passwd adr x -> do
            auth <- authCMI conf passwd
            writeCMI conf auth adr x
        ReadTo dbPath ->
            bracket (connectSqlite3 dbPath) disconnect $ \conn -> do
                dbTables <- getTables conn
                unless ("cmi" `elem` dbTables) $ createTable conn
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
    opts = info (((,) <$> confParser <*> actionParser) <**> helper) fullDesc
