module Lifx.Product where

import Data.Text (Text)
import Data.Word (Word16, Word32)

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
    { hev :: Bool
    , color :: Bool
    , chain :: Bool
    , matrix :: Bool
    , relays :: Bool
    , buttons :: Bool
    , infrared :: Bool
    , multizone :: Bool
    , temperatureRange :: Maybe (Word16, Word16)
    , extendedMultizone :: Bool
    }
    deriving (Show)

data Upgrade = Upgrade
    { major :: Word16
    , minor :: Word16
    , features :: PartialFeatures
    }
    deriving (Show)
