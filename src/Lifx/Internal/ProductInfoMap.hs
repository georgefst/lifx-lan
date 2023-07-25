module Lifx.Internal.ProductInfoMap where

import Control.Applicative
import Data.Functor
import Data.Maybe
import Data.Tuple.Extra
import Data.Word

import Data.Map (Map, (!?))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)

import Lifx.Internal.Product
import Lifx.Internal.ProductInfo

productInfoMap :: Map Word32 (Features, Map Word32 ProductInfo)
productInfoMap =
    Map.fromList $
        productInfo <&> \VendorInfo{..} ->
            ( vid
            ,
                ( defaults
                , Map.fromList $ ((.pid) &&& id) <$> products
                )
            )

-- | Information about a particular LIFX product.
data Product = Product
    { name :: Text
    , id :: Word32
    , features :: Features
    }
    deriving (Eq, Ord, Show, Generic)

data ProductLookupError
    = UnknownVendorId Word32
    | UnknownProductId Word32
    deriving (Eq, Ord, Show, Generic)

productLookup :: Word32 -> Word32 -> Word16 -> Word16 -> Either ProductLookupError Product
productLookup vendor prod versionMinor versionMajor =
    case productInfoMap !? vendor of
        Nothing -> Left $ UnknownVendorId vendor
        Just (defaults, products) -> case products !? prod of
            Nothing -> Left $ UnknownProductId prod
            Just ProductInfo{features = originalFeatures, ..} ->
                pure
                    Product
                        { name
                        , id = prod
                        , features =
                            completeFeatures defaults $
                                foldl
                                    ( \old Upgrade{..} ->
                                        if (versionMajor, versionMinor) >= (major, minor)
                                            then addFeatures features old
                                            else old
                                    )
                                    originalFeatures
                                    upgrades
                        }
  where
    completeFeatures f pf =
        Features
            { hev = fromMaybe f.hev pf.hev
            , color = fromMaybe f.color pf.color
            , chain = fromMaybe f.chain pf.chain
            , matrix = fromMaybe f.matrix pf.matrix
            , relays = fromMaybe f.relays pf.relays
            , buttons = fromMaybe f.buttons pf.buttons
            , infrared = fromMaybe f.infrared pf.infrared
            , multizone = fromMaybe f.multizone pf.multizone
            , temperatureRange = pf.temperatureRange <|> f.temperatureRange
            , extendedMultizone = fromMaybe f.extendedMultizone pf.extendedMultizone
            }
    -- left-biased
    addFeatures new old =
        PartialFeatures
            { hev = new.hev <|> old.hev
            , color = new.color <|> old.color
            , chain = new.chain <|> old.chain
            , matrix = new.matrix <|> old.matrix
            , relays = new.relays <|> old.relays
            , buttons = new.buttons <|> old.buttons
            , infrared = new.infrared <|> old.infrared
            , multizone = new.multizone <|> old.multizone
            , temperatureRange = new.temperatureRange <|> old.temperatureRange
            , extendedMultizone = new.extendedMultizone <|> old.extendedMultizone
            }
