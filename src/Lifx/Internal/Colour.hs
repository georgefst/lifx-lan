module Lifx.Internal.Colour where

import Control.Applicative
import Data.Colour.SRGB
import Data.Ord
import Data.Word

import Data.Colour.RGBSpace.HSV (hsv)
import Data.Colour.RGBSpace.HSV qualified as HSV

import Lifx.Lan.Internal

{- |
Note that when 'kelvin' has an effect (i.e. when saturation is any less than maximum), output is somewhat arbitrary.

LIFX's team have never shared an exact formula, and this implementation is inspired by various conflicting sources.
-}
hsbkToRgb :: HSBK -> RGB Float
hsbkToRgb HSBK{..} =
    interpolateColour
        (fromIntegral saturation / maxWord16)
        c
        c'
  where
    c =
        hsv
            (360 * fromIntegral hue / maxWord16)
            (fromIntegral saturation / maxWord16)
            (fromIntegral brightness / maxWord16)
    c' =
        let t =
                (log (fromIntegral kelvin) - log minKelvin)
                    / log (maxKelvin / minKelvin)
         in clamp (0, 1)
                <$> RGB
                    { channelRed = 1
                    , channelGreen = t / 2 + 0.5
                    , channelBlue = t
                    }

-- | Kelvin in output is always 0.
rgbToHsbk :: RGB Float -> HSBK
rgbToHsbk c =
    HSBK
        { hue = floor $ HSV.hue c * fromIntegral (maxBound @Word16 `div` 360)
        , saturation = floor $ HSV.saturation c * fromIntegral (maxBound @Word16)
        , brightness = floor $ HSV.value c * fromIntegral (maxBound @Word16)
        , kelvin = 0
        }

interpolateColour :: (Num a) => a -> RGB a -> RGB a -> RGB a
interpolateColour r = liftA2 (\a b -> a * (r + b * (1 - r)))

maxWord16 :: Float
maxWord16 = fromIntegral $ maxBound @Word16

minKelvin :: (Num a) => a
minKelvin = 1500

maxKelvin :: (Num a) => a
maxKelvin = 9000
