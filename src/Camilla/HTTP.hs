{-# LANGUAGE OverloadedStrings #-}
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

requestURL ip req = endpoint ++ "?jsonnode=" ++ show (jsonnode req) ++ "&jsonparam=" ++ show (jsonparam req)
    where endpoint = "http://" ++ show ip ++ "/INCLUDE/api.cgi"

readCMI :: HTTPConfig -> Request -> IO Response
readCMI conf req = do
    httpResponse <-
        getWith
            (defaults & auth ?~ basicAuth (cusername conf) (cpassword conf))
            (requestURL (cip conf) req)
    let json = httpResponse ^. responseBody
    either fail pure $ eitherDecode json
