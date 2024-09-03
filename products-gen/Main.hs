{-TODO ideally we'd make this a cabal script, but we want to depend on the local lifx-lan
so we'd need something like the ability to specify hs-source-dirs: https://github.com/haskell/cabal/issues/6787
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Aeson
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Deriving.Aeson
import Lifx.Internal.Product
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Ormolu
import System.Exit
import System.FilePath
import Text.Pretty.Simple

main :: IO ()
main = do
    resp <- httpLbs url =<< newManager tlsManagerSettings
    case eitherDecode @[VendorInfo] $ responseBody resp of
        Right xs -> do
            output <- ormolu defaultConfig "" $
                "-- | This is auto-generated - see the \"product-gen\" script.\n\
                \module Lifx.Internal.ProductInfo where\n\
                \\n\
                \import Lifx.Internal.Product\n\
                \\n\
                \productInfo :: [VendorInfo]\n\
                \productInfo =\n\
                \"
                    <> TL.toStrict (pShowOpt defaultOutputOptionsNoColor{outputOptionsInitialIndent = 4} xs)
                    <> "\n"
            T.writeFile ("src" </> "Lifx" </> "Internal" </> "ProductInfo.hs") output
        Left err -> putStrLn ("Decoding JSON failed: " <> err) >> exitFailure

commit :: String
commit = "95b87839fd91cfd187f6ed25a985f64643aeb1b5"
url :: Request
url = parseRequest_ $ "https://raw.githubusercontent.com/LIFX/products/" <> commit <> "/products.json"

type Opts = '[FieldLabelModifier '[CamelToSnake]]

deriving instance Generic VendorInfo
deriving via CustomJSON Opts VendorInfo instance FromJSON VendorInfo
deriving instance Generic ProductInfo
deriving via CustomJSON Opts ProductInfo instance FromJSON ProductInfo
deriving instance Generic PartialFeatures
deriving via CustomJSON Opts PartialFeatures instance FromJSON PartialFeatures
deriving via CustomJSON Opts Features instance FromJSON Features
deriving instance Generic Upgrade
deriving via CustomJSON Opts Upgrade instance FromJSON Upgrade
