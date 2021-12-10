module Lifx.Product where

import Data.Text (Text)

data Product = Product
    { pid :: Int
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
    , temperatureRange :: Maybe (Int, Int)
    , extendedMultizone :: Bool
    }
    deriving (Show)

data Upgrade = Upgrade
    { major :: Int
    , minor :: Int
    , features :: Features
    }
    deriving (Show)
