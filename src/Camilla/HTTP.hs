{-# LANGUAGE OverloadedStrings #-}
module Camilla.HTTP where

import qualified Network.Wreq as HTTP
import Camilla.Types
import Control.Lens
import Data.Aeson
import Data.Text (pack)

readCMI :: Request -> IO Response
readCMI req = do
    httpResponse <-
        HTTP.getWith
            (HTTP.defaults & HTTP.param "jsonnode" .~
             [pack . show $ jsonnode req] &
             HTTP.param "jsonparam" .~
             [pack . show $ jsonparam req])
            endpoint
    let bs = httpResponse ^. HTTP.responseBody
    either fail pure $ eitherDecode bs
  where
    endpoint = "http://cmi/INCLUDE/api.cgi"
