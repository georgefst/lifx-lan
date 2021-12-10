module Lifx.Product where

import Data.Text (Text)
import Data.Word (Word16, Word32)

data Product = Product
    { pid :: Word32
    , name :: Text
    , features :: Features
    , upgrades :: [Upgrade]
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
    , features :: Features
    }
    deriving (Show)
