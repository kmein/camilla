{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Camilla.HTTP
import Camilla.Types
import Data.HashMap.Strict (toList)
import Data.Monoid ((<>))
import Options.Applicative
import Text.PrettyPrint.Boxes hiding ((<>))
import Text.Printf

report :: Response -> Box
report = hsep 2 left . map toColumn . toList . rdata
  where
    toColumn (k, v) = align center1 center1 (text $ show k) /+/ vcat left (map toRow v)
    toRow DataPoint {..} =
        text (printf "%2d" dnumber ++ ":") <+> text val <+> text (show $ vunit dvalue)
      where
        val =
            case dvalue of
                DigitalValue {bvalue = x} -> show x
                AnalogValue {avalue = x} -> show x

confParser :: Parser HTTPConfig
confParser =
    HTTPConfig <$>
    option
        auto
        (long "ip" <> short 'i' <> metavar "ADDR" <> help "CMI IP address") <*>
    strOption
        (long "user" <> short 'u' <> metavar "NAME" <> help "CMI username" <>
         value "admin") <*>
    strOption
        (long "pass" <> short 'p' <> metavar "PASS" <> help "CMI password" <>
         value "admin")

main :: IO ()
main = do
    conf <- execParser opts
    readCMIWithInterval 30 conf req $ printBox . report
    where req = Request 1 [JSONParam Inputs Nothing, JSONParam Outputs Nothing]
          opts = info (confParser <**> helper) fullDesc
