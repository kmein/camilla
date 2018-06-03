{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Camilla.Types
  ( Request(..)
  , Response(..)
  , JSONParam(..)
  , JSONParamType(..)
  , Header(..)
  , Version(..)
  , Device(..)
  , DataPoint(..)
  , Data(..)
  , DataValue(..)
  , Unit(..)
  , RAS(..)
  , Status(..)
  ) where

import Camilla.Util
import Control.Applicative (optional)
import Control.Arrow ((***))
import Data.Aeson hiding (Error)
import Data.Aeson.Casing (aesonDrop, pascalCase)
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)
import Numeric (readHex, showHex)
import Numeric.Natural (Natural)
import qualified Data.HashMap.Strict as M ((!), lookup, HashMap, toList, fromList)
import Data.Text (unpack, Text)

camillaCase = aesonDrop 1 pascalCase

data Request = Request
    { jsonnode :: Natural
    , jsonparam :: [JSONParam]
    } deriving (Eq, Show)

data JSONParam = JSONParam
    { ptype :: JSONParamType
    , pnumber :: Maybe Int
    } deriving (Eq)

instance Show JSONParam where
    show p = show (ptype p) ++ maybe "" show (pnumber p)
    showList ps _ = intercalate "," $ map show ps

data JSONParamType
    = Inputs
    | Outputs
    | DLInputs
    | SystemGeneral
    | SystemDate
    | SystemTime
    | SystemSun
    | SystemPower
    | NetworkAnalog
    | NetworkDigital
    | MBus
    | Modbus
    | KNX
    | LoggingAnalog
    | LoggingDigital
     deriving (Eq, Generic)

instance Hashable JSONParamType

_jsonParamTypeString = [(Inputs, "I"), (Outputs, "O"), (DLInputs, "D"), (SystemGeneral, "Sg"), (SystemDate, "Sd"), (SystemTime, "St"), (SystemSun, "Ss"), (SystemPower, "Sp"), (NetworkAnalog, "Na"), (NetworkDigital, "Nd"), (MBus, "M"), (Modbus, "AM"), (KNX, "AK"), (LoggingAnalog, "La"), (LoggingDigital, "Ld")]
instance Show JSONParamType where
    show p = fromJust (lookup p _jsonParamTypeString)

parseJSONParamType :: (Monad m) => Text -> m JSONParamType
parseJSONParamType = \case
    "Inputs" -> pure Inputs
    "Outputs" -> pure Outputs
    _ -> fail "jsonparam not defined."

data Response = Response
    { rheader :: Header
    , rdata :: Data
    , rstatus :: Status
    } deriving (Eq, Show)

instance FromJSON Response where
    parseJSON = withObject "Response" $ \v -> Response
        <$> v .: "Header"
        <*> v .: "Data"
        <*> fmap toEnum (v .: "Status code")

data Header = Header
    { hversion :: Version
    , hdevice :: Device
    , htimestamp :: POSIXTime
    } deriving (Eq, Show, Generic)

instance FromJSON Header where
    parseJSON = genericParseJSON camillaCase

data Version
    = V1_25_2
    | V1_26_1
    | V1_28_0
     deriving (Bounded, Eq, Ord, Show)

instance FromJSON Version where
    parseJSON = withScientific "Version" $ pure . toEnum . truncate

_versionNumber = [(V1_25_2, 1), (V1_26_1, 2), (V1_28_0, 3)]
instance Enum Version where
    fromEnum v = fromJust $ lookup v _versionNumber
    toEnum n = fromJust $ lookup n $ map (\(v, x) -> (x, v)) _versionNumber

data Device
    = CoE
    | UVR1611
    | CAN_MT
    | CAN_IO44
    | CAN_IO35
    | CAN_BC
    | CAN_EZ
    | CAN_TOUCH
    | UVR16x2
    | RSM610
    | CAN_IO45
    | CMI
    | CAN_EZ2
    | CAN_MTx2
    | CAN_BC2
    | BL_NET
     deriving (Eq, Show)

instance FromJSON Device where
    parseJSON = withText "Device" $ \t ->
         let [(v, [])] = readHex $ unpack t
         in pure $ toEnum v

_deviceNumber = [(CoE, 0x7f), (UVR1611, 0x80), (CAN_MT, 0x81), (CAN_IO44, 0x82), (CAN_IO35, 0x83), (CAN_BC, 0x84), (CAN_EZ, 0x85), (CAN_TOUCH, 0x86), (UVR16x2, 0x87), (RSM610, 0x88), (CAN_IO45, 0x89), (CMI, 0x8a), (CAN_EZ2, 0x8b), (CAN_MTx2, 0x8c), (CAN_BC2, 0x8d), (BL_NET, 0xa3)]
instance Enum Device where
    fromEnum d = fromJust $ lookup d _deviceNumber
    toEnum n = fromJust $ lookup n $ map (\(d, x) -> (x, d)) _deviceNumber

newtype Data = Data
    { dvalues :: M.HashMap JSONParamType [DataPoint]
    } deriving (Eq, Show)

instance FromJSON Data where
    parseJSON = withObject "Data" $ fmap Data . traverseKVs parseJSONParamType parseJSON

data DataPoint = DataPoint
    { dnumber :: Natural
    , dvalue :: DataValue
    } deriving (Eq, Show, Generic)

instance FromJSON DataPoint where
    parseJSON = withObject "DataPoint" $ \d -> do
        number <- d .: "Number"
        ad <- d .: "AD"
        v <- d .: "Value"
        value <- v .: "Value"
        unit <- v .: "Unit"
        case ad :: Text of
            "A" -> do
                state <- optional $ truthy <$> v .: "State"
                pure DataPoint
                    { dnumber = number
                    , dvalue =
                        AnalogValue
                        { avalue = value
                        , vunit = unit
                        , vstate = state
                        }
                    }
            "D" ->
                pure DataPoint
                    { dnumber = number
                    , dvalue =
                        DigitalValue
                        { bvalue = truthy value
                        , vunit = unit
                        }
                    }
      where
        truthy = \case 1 -> True; 0 -> False

data DataValue
    = AnalogValue { avalue :: Double
                  , vunit :: Unit
                  , vstate :: Maybe Bool}
    | DigitalValue { bvalue :: Bool
                   , vunit :: Unit}
     deriving (Eq, Show, Generic)

data Unit
    = Unit0
    | C1
    | Wm2
    | LH
    | Sec4
    | Min
    | LImp
    | K
    | Percent8
    | KW
    | KWh
    | MWh
    | V
    | MA
    | Hr
    | Days
    | Imp
    | Kohm
    | L
    | KmH
    | Hz
    | LMin
    | Bar
    | Unit24
    | Km
    | M
    | Mm
    | M3
    | LD
    | MS
    | M3Min
    | M3H
    | M3D
    | MmMin
    | MmH
    | MmD
    | OnOff
    | NoYes
    | C46 RAS
    | Euro
    | Dollar
    | GM3
    | Unit53
    | Degree54
    | Degree56
    | Sec57
    | Unit58
    | Percent59
    | H
    | A
    | Mbar
    | Pa
    | Ppm
    deriving (Eq)

_unitNumber = [(Unit0, 0), (C1, 1), (Wm2, 2), (LH, 3), (Sec4, 4), (Min, 5), (LImp, 6), (K, 7), (Percent8, 8), (KW, 10), (KWh, 11), (MWh, 12), (V, 13), (MA, 14), (Hr, 15), (Days, 16), (Imp, 17), (Kohm, 18), (L, 19), (KmH, 20), (Hz, 21), (LMin, 22), (Bar, 23), (Unit24, 24), (Km, 25), (M, 26), (Mm, 27), (M3, 28), (LD, 35), (MS, 36), (M3Min, 37), (M3H, 38), (M3D, 39), (MmMin, 40), (MmH, 41), (MmD, 42), (OnOff, 43), (NoYes, 44), (Euro, 50), (Dollar, 51), (GM3, 52), (Unit53, 53), (Degree54, 54), (Degree56, 56), (Sec57, 57), (Unit58, 58), (Percent59, 59), (H, 60), (A, 63), (Mbar, 65), (Pa, 66), (Ppm, 67)] ++ map (\r -> (C46 r, 46)) [minBound..maxBound]
instance Enum Unit where
    fromEnum u = fromJust $ lookup u _unitNumber
    toEnum n = fromJust $ lookup n $ map (\(u, x) -> (x, u)) _unitNumber

instance Show Unit where
    show = \case
        u   | Just name <- lookup u _unitName -> name
            | u `elem` [Unit0, Unit24, Unit53, Unit58] -> ""
            | C46 r <- u -> "°C (" ++ show r ++ ")"
            | u `elem` [Percent8, Percent59] -> "%"
            | u `elem` [Sec4, Sec57] -> "sec"
            | u `elem` [Degree54, Degree56] -> "°"
        where _unitName = [(C1, "°C"), (Wm2, "W/m²"), (LH, "l/h"), (Min, "min"), (LImp, "l/Imp"), (K, "K"), (KW, "kW"), (KWh, "kWh"), (MWh, "MWh"), (V, "V"), (MA, "mA"), (Hr, "hr"), (Days, "Days"), (Imp, "Imp"), (Kohm, "kΩ"), (L, "l"), (KmH, "km/h"), (Hz, "Hz"), (LMin, "l/min"), (Bar, "bar"), (Km, "km"), (M, "m"), (Mm, "mm"), (M3, "m³"), (LD, "l/d"), (MS, "m/s"), (M3Min, "m³/min"), (M3H, "m³/h"), (M3D, "m³/d"), (MmMin, "mm/min"), (MmH, "mm/h"), (MmD, "mm/d"), (OnOff, "ON/OFF"), (NoYes, "NO/YES"), (Euro, "€"), (Dollar, "$"), (GM3, "g/m³"), (H, "h"), (A, "A"), (Mbar, "mbar"), (Pa, "Pa"), (Ppm, "ppm")]

instance FromJSON Unit where
    parseJSON = \case
        String v -> pure . toEnum . read $ unpack v
        _ -> fail "Unit must be a numeric string."

data RAS
    = Auto
    | Standard
    | Setback
    | Standby
     deriving (Show, Eq, Enum, Bounded)

data Status
    = Ok
    | NodeError
    | Fail
    | SyntaxError
    | TooManyRequests
    | DeviceNotSupported
    | TooFewArguments
    | Error Int
     deriving (Eq, Ord, Show)

_statusNumber = [(Ok, 0), (NodeError, 1), (Fail, 2), (SyntaxError, 3), (TooManyRequests, 4), (DeviceNotSupported, 5), (TooFewArguments, 6)]
instance Enum Status where
    fromEnum s
        | Error n <- s = n
        | otherwise = fromJust $ lookup s _statusNumber
    toEnum n
        | n <= 6 = fromJust $ lookup n $ map (\(s, x) -> (x, s)) _statusNumber
        | otherwise = Error n
