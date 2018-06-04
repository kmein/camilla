{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
import Camilla.Types
import Data.Aeson.Types
import Data.Aeson.QQ
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

main :: IO ()
main = do
    runTestTT $ TestList [parseHeader, parseData, parseResponse]
    pure ()
