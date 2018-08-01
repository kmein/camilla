{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Camilla.HTTP where

import Camilla.Types

import Control.Lens
import Control.Monad (void)
import Data.Aeson
import qualified Data.ByteString as B (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy as BL (pack, unpack)
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import Data.IP (IP)
import Data.Text (pack)
import Data.Time.Clock
import Network.HTTP.Client (CookieJar, createCookieJar, Cookie(..))
import Network.Wreq hiding (Response)

data HTTPConfig = HTTPConfig
    { cip :: IP
    , cusername :: B.ByteString
    , cpassword :: B.ByteString
    } deriving (Eq, Show)

defaultHTTPConfig :: IP -> HTTPConfig
defaultHTTPConfig ip =
    HTTPConfig
    { cip = ip
    , cusername = "admin"
    , cpassword = "admin"
    }

apiURL :: IP -> Request -> String
apiURL ip Request{..} = endpoint ++ "?jsonnode=" ++ show jsonnode ++ "&jsonparam=" ++ show jsonparam
    where endpoint = "http://" ++ show ip ++ "/INCLUDE/api.cgi"

expertURL :: IP -> String -> String
expertURL ip password = endpoint ++ "?changeuserx2=010257ff" ++ password
    where endpoint = "http://" ++ show ip ++ "/INCLUDE/devpagexUser.cgi"

changeURL :: IP -> String -> Either Int Double -> String
changeURL ip adr x = endpoint ++ "?changeadrx2=" ++ adr ++ "&changetox2=" ++ either show show x
    where endpoint = "http://" ++ show ip ++ "/INCLUDE/change.cgi"

authCMI :: HTTPConfig -> String -> IO CookieJar
authCMI HTTPConfig {..} password = do
    resp <-
        getWith (defaults & auth ?~ basicAuth cusername cpassword) $
        expertURL cip password
    let body = resp ^. responseBody
    now <- getCurrentTime
    return $
        createCookieJar
            [ Cookie
                  "canremote1"
                  (B.pack $ BL.unpack body)
                  (addUTCTime (30 * nominalDay) now)
                  (BC.pack $ show cip)
                  "/"
                  now
                  now
                  True
                  True
                  False
                  False]

writeCMI :: HTTPConfig -> CookieJar -> String -> Either Int Double -> IO ()
writeCMI HTTPConfig {..} jar adr x =
    void $
    getWith
        (defaults & auth ?~ basicAuth cusername cpassword & cookies ?~ jar) $
    changeURL cip adr x

readCMI :: HTTPConfig -> Request -> IO Response
readCMI HTTPConfig {..} req = do
    httpResponse <-
        getWith (defaults & auth ?~ basicAuth cusername cpassword) $
        apiURL cip req
    let json = httpResponse ^. responseBody
    either fail pure $ eitherDecode json
