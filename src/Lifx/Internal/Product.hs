-- | An exact mapping of the types in LIFX's `products.json`. It's easier to just use 'Lifx.Lan.getProductInfo'.
module Lifx.Internal.Product where

import Data.Text (Text)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)

data VendorInfo = VendorInfo
    { vid :: Word32
    , name :: Text
    , defaults :: Features
    , products :: [ProductInfo]
    }
    deriving (Show)

data ProductInfo = ProductInfo
    { pid :: Word32
    , name :: Text
    , features :: PartialFeatures
    , upgrades :: [Upgrade]
    }
    deriving (Show)

data PartialFeatures = PartialFeatures
    { hev :: Maybe Bool
    , color :: Maybe Bool
    , chain :: Maybe Bool
    , matrix :: Maybe Bool
    , relays :: Maybe Bool
    , buttons :: Maybe Bool
    , infrared :: Maybe Bool
    , multizone :: Maybe Bool
    , temperatureRange :: Maybe (Word16, Word16)
    , extendedMultizone :: Maybe Bool
    }
    deriving (Show)

data Features = Features
    { -- | The light supports emitting HEV light
      hev :: Bool
    , -- | The light changes physical appearance when the Hue value is changed
      color :: Bool
    , -- | The light may be connected to physically separated hardware (currently only the LIFX Tile)
      chain :: Bool
    , -- | The light supports a 2D matrix of LEDs (the Tile and Candle)
      matrix :: Bool
    , -- | The device has relays for controlling physical power to something (the LIFX Switch)
      relays :: Bool
    , -- | The device has physical buttons to press (the LIFX Switch)
      buttons :: Bool
    , -- | The light supports emitting infrared light
      infrared :: Bool
    , -- | The light supports a 1D linear array of LEDs (the Z and Beam)
      multizone :: Bool
    , -- | An array of the minimum and maximum kelvin values this device supports. If the numbers are the same then the device does not support variable kelvin values. It is null for devices that aren't lighting products (the LIFX Switch)
      temperatureRange :: Maybe (Word16, Word16)
    , -- | The more capable extended API for multizone control that lets us control all the zones on the device with a single message instead of many.
      extendedMultizone :: Bool
    }
    deriving (Eq, Ord, Show, Generic)

data Upgrade = Upgrade
    { major :: Word16
    , minor :: Word16
    , features :: PartialFeatures
    }
    deriving (Show)
