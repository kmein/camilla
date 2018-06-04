{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Camilla.HTTP where

import Network.Wreq hiding (Response)
import Camilla.Types
import Control.Lens
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (pack)
import Data.IP (IP)

data HTTPConfig = HTTPConfig
    { cip :: IP
    , cusername :: ByteString
    , cpassword :: ByteString
    } deriving (Eq, Show)

defaultHTTPConfig :: IP -> HTTPConfig
defaultHTTPConfig ip =
    HTTPConfig
    { cip = ip
    , cusername = "admin"
    , cpassword = "admin"
    }

requestURL :: IP -> Request -> String
requestURL ip Request{..} = endpoint ++ "?jsonnode=" ++ show jsonnode ++ "&jsonparam=" ++ show jsonparam
    where endpoint = "http://" ++ show ip ++ "/INCLUDE/api.cgi"

readCMI :: HTTPConfig -> Request -> IO Response
readCMI HTTPConfig{..} req = do
    httpResponse <-
        getWith
            (defaults & auth ?~ basicAuth cusername cpassword)
            (requestURL cip req)
    let json = httpResponse ^. responseBody
    either fail pure $ eitherDecode json
