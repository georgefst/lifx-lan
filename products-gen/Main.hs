{-TODO ideally we'd make this a cabal script, but we want to depend on the local lifx-lan
so we'd need something like the ability to specify hs-source-dirs: https://github.com/haskell/cabal/issues/6787
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative
import Data.Aeson
import Data.Functor
import Data.Maybe
import Data.Text (Text)
import Data.Word
import GHC.Generics
import Lifx.Product
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Exit
import Text.Pretty.Simple
import System.FilePath

main :: IO ()
main = do
    man <- newManager tlsManagerSettings
    resp <- httpLbs url man
    case eitherDecode @[_] $ responseBody resp of
        Right xs -> do
            let out =
                    pShowOpt defaultOutputOptionsNoColor{outputOptionsInitialIndent = 4} $
                        xs
                            <&> \Data
                                    { defaults =
                                        Features''
                                            { hev = hevDefault
                                            , color = colorDefault
                                            , chain = chainDefault
                                            , matrix = matrixDefault
                                            , relays = relaysDefault
                                            , buttons = buttonsDefault
                                            , infrared = infraredDefault
                                            , multizone = multizoneDefault
                                            , temperature_range = temperatureRangeDefault
                                            , extended_multizone = extendedMultizoneDefault
                                            }
                                    , name = vendorName
                                    , ..
                                    } ->
                                    let fillDefaults = \Features'{..} ->
                                            Features
                                                { hev = fromMaybe hevDefault hev
                                                , color = fromMaybe colorDefault color
                                                , chain = fromMaybe chainDefault chain
                                                , matrix = fromMaybe matrixDefault matrix
                                                , relays = fromMaybe relaysDefault relays
                                                , buttons = fromMaybe buttonsDefault buttons
                                                , infrared = fromMaybe infraredDefault infrared
                                                , multizone = fromMaybe multizoneDefault multizone
                                                , temperatureRange = temperature_range <|> temperatureRangeDefault
                                                , extendedMultizone = fromMaybe extendedMultizoneDefault extended_multizone
                                                }
                                     in ( (vid, vendorName)
                                        , products <&> \Product'{..} ->
                                            Product
                                                { features = fillDefaults features
                                                , upgrades =
                                                    upgrades <&> \Upgrade'{features = upgradeFeatures, ..} ->
                                                        Upgrade
                                                            { features = fillDefaults upgradeFeatures
                                                            , ..
                                                            }
                                                , ..
                                                }
                                        )
            TL.writeFile ("src" </> "Lifx" </> "ProductInfo.hs") $
                "module Lifx.ProductInfo where\n\
                \\n\
                \import Data.Map qualified as Map\n\
                \import Data.Text (Text)\n\
                \import Data.Word (Word32)\n\
                \\n\
                \import Lifx.Product\n\
                \\n\
                \productInfo :: Map.Map (Word32, Text) [Product]\n\
                \productInfo = Map.fromList\n\
                \"
                    <> out
                    <> "\n"
        Left err -> putStrLn ("Decoding JSON failed: " <> err) >> exitFailure

commit :: String
commit = "dd692a341125a1c7b7397e058ee0d0f20f120a36"
url :: Request
url = parseRequest_ $ "https://raw.githubusercontent.com/LIFX/products/" <> commit <> "/products.json"

data Data = Data
    { vid :: Int
    , name :: Text
    , defaults :: Features''
    , products :: [Product']
    }
    deriving (Show, Generic, FromJSON)
data Product' = Product'
    { pid :: Word32
    , name :: Text
    , features :: Features'
    , upgrades :: [Upgrade']
    }
    deriving (Show, Generic, FromJSON)
data Features' = Features'
    { hev :: Maybe Bool
    , color :: Maybe Bool
    , chain :: Maybe Bool
    , matrix :: Maybe Bool
    , relays :: Maybe Bool
    , buttons :: Maybe Bool
    , infrared :: Maybe Bool
    , multizone :: Maybe Bool
    , temperature_range :: Maybe (Word16, Word16)
    , extended_multizone :: Maybe Bool
    }
    deriving (Show, Generic, FromJSON)
data Features'' = Features''
    { hev :: Bool
    , color :: Bool
    , chain :: Bool
    , matrix :: Bool
    , relays :: Bool
    , buttons :: Bool
    , infrared :: Bool
    , multizone :: Bool
    , temperature_range :: Maybe (Word16, Word16)
    , extended_multizone :: Bool
    }
    deriving (Show, Generic, FromJSON)
data Upgrade' = Upgrade'
    { major :: Word16
    , minor :: Word16
    , features :: Features'
    }
    deriving (Show, Generic, FromJSON)
