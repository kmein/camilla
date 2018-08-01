{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Camilla.HTTP where

import Camilla.Types

import Control.Lens
import Control.Monad (void)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.IP (IP)
import Data.Text (pack)
import Network.Wreq hiding (Response)
import qualified Network.Wreq.Session as S

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

apiURL :: IP -> Request -> String
apiURL ip Request{..} = endpoint ++ "?jsonnode=" ++ show jsonnode ++ "&jsonparam=" ++ show jsonparam
    where endpoint = "http://" ++ show ip ++ "/INCLUDE/api.cgi"

expertURL :: IP -> String -> String
expertURL ip password = endpoint ++ "?changeuserx2=010257ff" ++ password
    where endpoint = "http://" ++ show ip ++ "/INCLUDE/devpagexUser.cgi"

changeURL :: IP -> String -> Either Int Double -> String
changeURL ip adr x = endpoint ++ "?changeadrx2=" ++ adr ++ "&changetox2=" ++ either show show x
    where endpoint = "http://" ++ show ip ++ "/INCLUDE/change.cgi"

authCMI :: S.Session -> HTTPConfig -> String -> IO ()
authCMI sess HTTPConfig {..} password =
    void $
    S.getWith (defaults & auth ?~ basicAuth cusername cpassword) sess $
    expertURL cip password

writeCMI :: S.Session -> HTTPConfig -> String -> Either Int Double -> IO ()
writeCMI sess HTTPConfig {..} adr x =
    void $
    S.getWith (defaults & auth ?~ basicAuth cusername cpassword) sess $
    changeURL cip adr x

readCMI :: HTTPConfig -> Request -> IO Response
readCMI HTTPConfig {..} req = do
    httpResponse <-
        getWith (defaults & auth ?~ basicAuth cusername cpassword) $
        apiURL cip req
    let json = httpResponse ^. responseBody
    either fail pure $ eitherDecode json
