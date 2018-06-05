{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
import Camilla.Types
import Camilla.HTTP
import Data.Aeson.Types
import Data.Aeson.QQ
import Database.HDBC
import Test.HUnit
import qualified Data.HashMap.Strict as M

parseHeader =
    TestLabel "parseHeader" $
    TestCase $
    Success
        Header
        { hversion = V1_25_2
        , hdevice = CAN_EZ2
        , htimestamp = 1481538940
        } @=?
    fromJSON [aesonQQ| {
        "Version":1,
        "Device":"8B",
        "Timestamp":1481538940
    } |]

parseData =
    TestLabel "parseData" $
    TestCase $
    Success
        ([ ( Inputs
           , [ DataPoint
                   1
                   AnalogValue
                   { avalue = 22.7
                   , vunit = C1
                   , vstate = Nothing
                   }])
         , ( Outputs
           , [ DataPoint
                   1
                   AnalogValue
                   { vstate = Just False
                   , avalue = 0
                   , vunit = Unit0
                   }])] :: M.HashMap JSONParamType [DataPoint]) @=?
    fromJSON
        [aesonQQ| {
           "Inputs":[
              {
                 "Number":1,
                 "AD":"A",
                 "Value":{
                    "Value":22.7,
                    "Unit":"1"
                 }
              }
           ],
           "Outputs":[
              {
                 "Number":1,
                 "AD":"A",
                 "Value":{
                    "State":0,
                    "Value":0,
                    "Unit":"0"
                 }
              }
           ]
        } |]


parseResponse =
    TestLabel "parseResponse" $
    TestCase $
    Success
        Response
        { rheader =
            Header
            { hversion = V1_25_2
            , hdevice = UVR16x2
            , htimestamp = 1481546305
            }
        , rdata =
            [ ( Inputs
              , [ DataPoint
                      1
                      AnalogValue
                      { avalue = 92
                      , vunit = C1
                      , vstate = Nothing
                      }
                , DataPoint
                      2
                      AnalogValue
                      { avalue = 71.2
                      , vunit = C1
                      , vstate = Nothing
                      }
                , DataPoint
                      14
                      AnalogValue
                      { avalue = 45.8
                      , vunit = C46 Auto
                      , vstate = Nothing
                      }])
            , ( Outputs
              , [ DataPoint
                      1
                      DigitalValue
                      { bvalue = True
                      , vunit = OnOff
                      }
                , DataPoint
                      2
                      DigitalValue
                      { bvalue = False
                      , vunit = OnOff
                      }
                , DataPoint
                      7
                      DigitalValue
                      { bvalue = False
                      , vunit = OnOff
                      }])]
        , rstatus = Ok
        } @=?
    fromJSON
        [aesonQQ| {
       "Header":{
          "Version":1,
          "Device":"87",
          "Timestamp":1481546305
       },
       "Data":{
          "Inputs":[
             {
                "Number":1,
                "AD":"A",
                "Value":{
                   "Value":92.0,
                   "Unit":"1"
                }
             },
             {
                "Number":2,
                "AD":"A",
                "Value":{
                   "Value":71.2,
                   "Unit":"1"
                }
             },
             {
                "Number":14,
                "AD":"A",
                "Value":{
                   "Value":45.8,
                   "Unit":"46",
                   "RAS":"0"
                }
             }
          ],
          "Outputs":[
             {
                "Number":1,
                "AD":"D",
                "Value":{
                   "Value":1,
                   "Unit":"43"
                }
             },
             {
                "Number":2,
                "AD":"D",
                "Value":{
                   "Value":0,
                   "Unit":"43"
                }
             },
             {
                "Number":7,
                "AD":"D",
                "Value":{
                   "Value":0,
                   "Unit":"43"
                }
             }
          ]
       },
       "Status":"OK",
       "Status code":0
    } |]

generateURL =
    TestLabel "generateURL" $
    TestCase $
    [ "http://192.168.178.20/INCLUDE/api.cgi?jsonnode=1&jsonparam=La,Ld"
    , "http://192.168.178.20/INCLUDE/api.cgi?jsonnode=2&jsonparam=I1,O2,O3"
    , "http://192.168.178.20/INCLUDE/api.cgi?jsonnode=1&jsonparam=Ss"] @=?
    map
        (requestURL $ read "192.168.178.20")
        [ Request
              1
              [ JSONParam LoggingAnalog Nothing
              , JSONParam LoggingDigital Nothing]
        , Request
              2
              [ JSONParam Inputs (Just 1)
              , JSONParam Outputs (Just 2)
              , JSONParam Outputs (Just 3)]
        , Request 1 [JSONParam SystemSun Nothing]]

generateSQL =
    TestLabel "generateSQL" $
    TestCase $
    [ [ SqlString "1.25.2"
      , SqlString "CMI"
      , SqlPOSIXTime 1
      , SqlString "I"
      , SqlInteger 1
      , SqlDouble 1.2
      , SqlString "m"]
    , [ SqlString "1.25.2"
      , SqlString "CMI"
      , SqlPOSIXTime 1
      , SqlString "O"
      , SqlInteger 2
      , SqlBool True
      , SqlString "NO/YES"]] @=?
    toSqlRows
        Response
        { rheader =
            Header
            { hversion = V1_25_2
            , hdevice = CMI
            , htimestamp = 1
            }
        , rdata =
            [ ( Inputs
              , [ DataPoint
                      1
                      AnalogValue
                      { avalue = 1.2
                      , vunit = M
                      , vstate = Nothing
                      }])
            , ( Outputs
              , [ DataPoint
                      2
                      DigitalValue
                      { bvalue = True
                      , vunit = NoYes
                      }])]
        , rstatus = Ok
        }

main :: IO ()
main = do
    runTestTT $ TestList [parseHeader, parseData, parseResponse, generateURL, generateSQL]
    pure ()
