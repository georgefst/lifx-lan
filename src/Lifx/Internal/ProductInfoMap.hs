module Lifx.Internal.ProductInfoMap where

import Control.Applicative
import Data.Either.Extra
import Data.Foldable hiding (product)
import Data.Function
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

-- TODO RecordDotSyntax can make this and other hiding unnecessary (we could also use "id" instead of "productId"...)
import Prelude hiding (product)

productInfoMap :: Map Word32 (Features, Map Word32 ProductInfo)
productInfoMap =
    Map.fromList $
        productInfo <&> \VendorInfo{..} ->
            ( vid
            ,
              ( defaults
              , Map.fromList $ (pid &&& id) <$> products
              )
            )

-- | Information about a particular LIFX product.
data Product = Product
    { name :: Text
    , productId :: Word32
    , features :: Features
    }
    deriving (Eq, Ord, Show, Generic)

data ProductLookupError
    = UnknownVendorId Word32
    | UnknownProductId Word32
    deriving (Eq, Ord, Show, Generic)

productLookup :: Word32 -> Word32 -> Word16 -> Word16 -> Either ProductLookupError Product
productLookup vendor product versionMinor versionMajor =
    case productInfoMap !? vendor of
        Nothing -> Left $ UnknownVendorId vendor
        Just (defaults, products) -> case products !? product of
            Nothing -> Left $ UnknownProductId product
            Just ProductInfo{features = originalFeatures, ..} ->
                pure
                    Product
                        { name
                        , productId = product
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
    -- TODO RecordDotSyntax
    completeFeatures
        Features
            { ..
            }
        PartialFeatures
            { hev = maybe_hev
            , color = maybe_color
            , chain = maybe_chain
            , matrix = maybe_matrix
            , relays = maybe_relays
            , buttons = maybe_buttons
            , infrared = maybe_infrared
            , multizone = maybe_multizone
            , temperatureRange = maybe_temperatureRange
            , extendedMultizone = maybe_extendedMultizone
            } =
            Features
                { hev = fromMaybe hev maybe_hev
                , color = fromMaybe color maybe_color
                , chain = fromMaybe chain maybe_chain
                , matrix = fromMaybe matrix maybe_matrix
                , relays = fromMaybe relays maybe_relays
                , buttons = fromMaybe buttons maybe_buttons
                , infrared = fromMaybe infrared maybe_infrared
                , multizone = fromMaybe multizone maybe_multizone
                , temperatureRange = maybe_temperatureRange <|> temperatureRange
                , extendedMultizone = fromMaybe extendedMultizone maybe_extendedMultizone
                }
    -- left-biased
    addFeatures
        PartialFeatures
            { ..
            }
        PartialFeatures
            { hev = old_hev
            , color = old_color
            , chain = old_chain
            , matrix = old_matrix
            , relays = old_relays
            , buttons = old_buttons
            , infrared = old_infrared
            , multizone = old_multizone
            , temperatureRange = old_temperatureRange
            , extendedMultizone = old_extendedMultizone
            } =
            PartialFeatures
                { hev = hev <|> old_hev
                , color = color <|> old_color
                , chain = chain <|> old_chain
                , matrix = matrix <|> old_matrix
                , relays = relays <|> old_relays
                , buttons = buttons <|> old_buttons
                , infrared = infrared <|> old_infrared
                , multizone = multizone <|> old_multizone
                , temperatureRange = temperatureRange <|> old_temperatureRange
                , extendedMultizone = extendedMultizone <|> old_extendedMultizone
                }
