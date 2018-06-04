module Main where

import Camilla.Types
import Camilla.HTTP
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.Monoid ((<>))
import Options.Applicative

confParser :: Parser HTTPConfig
confParser =
    HTTPConfig <$>
    option
        auto
        (long "ip" <> short 'i' <> metavar "ADDR" <> help "CMI IP address") <*>
    strOption
        (long "user" <> short 'u' <> metavar "NAME" <> help "CMI username") <*>
    strOption
        (long "pass" <> short 'p' <> metavar "PASS" <> help "CMI password")

-- reliable interval: 24.5 s
readWithInterval :: Double -> HTTPConfig -> Request -> (Response -> IO a) -> IO ()
readWithInterval secs conf req f = forever $ do
    f =<< readCMI conf req
    threadDelay (truncate $ secs * 1000000)

main :: IO ()
main = do
    conf <- execParser opts
    bracket undefined undefined $
        \conn -> readWithInterval 24.5 conf req $ \resp -> undefined resp
  where
    req = Request 1 [JSONParam Inputs Nothing, JSONParam Outputs Nothing]
    opts = info (confParser <**> helper) fullDesc
