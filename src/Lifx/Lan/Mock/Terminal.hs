module Lifx.Lan.Mock.Terminal (Mock, runMock) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Data.Foldable
import Data.Ord
import Data.Tuple.Extra
import Data.Word

import Data.Colour.RGBSpace.HSV (hsv)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import System.Console.ANSI hiding (SetColor)

import Lifx.Lan

newtype Mock a = Mock {unMock :: StateT (Map Device (Bool, HSBK)) (ReaderT [Device] IO) a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader [Device]
        , MonadState (Map Device (Bool, HSBK))
        )

runMock :: [Device] -> Mock a -> IO a
runMock ds = flip runReaderT ds . flip evalStateT (Map.fromList $ zip ds $ repeat (True, HSBK 0 0 0 0)) . (.unMock)

instance MonadLifx Mock where
    sendMessage d = \case
        GetService -> err
        GetHostFirmware -> err
        GetPower -> err
        SetPower b -> do
            modify $ Map.update (pure . first (const b)) d
            out
        GetVersion -> err
        GetColor -> err
        SetColor c _t -> do
            modify $ Map.update (pure . second (const c)) d
            out
        SetLightPower _ _ -> err >> out
      where
        err = error "message unimplemented" -- TODO
        out = do
            ds <- ask
            for_ ds \d' -> do
                sgr <- gets $ maybe [] mkSGR . Map.lookup d'
                liftIO do
                    setSGR sgr
                    putStr
                        . maybe ("error: couldn't get terminal size") (flip replicate ' ' . (`div` length ds) . snd)
                        =<< getTerminalSize
                    setSGR []
            liftIO $ putStrLn ""
          where
            mkSGR (p, c) =
                if p
                    then [SetRGBColor Background . uncurryRGB sRGB $ hsbkToRgb c]
                    else []
    broadcastMessage m = ask >>= traverse \d -> (d,) <$> sendMessage d m
    discoverDevices x = maybe id take x <$> ask
    lifxThrow = error . show -- TODO I'm sure we can do better

-- TODO duplicated from lifx-manager - put this somewhere useful
hsbkToRgb :: HSBK -> RGB Float
hsbkToRgb HSBK{..} =
    interpolateColour
        (fromIntegral saturation / maxWord16)
        c
        c'
  where
    interpolateColour :: Num a => a -> RGB a -> RGB a -> RGB a
    interpolateColour r = liftA2 (\a b -> a * (r + b * (1 - r)))
    maxWord16 :: Float
    maxWord16 = fromIntegral $ maxBound @Word16
    minKelvin :: Num a => a
    minKelvin = 1500
    maxKelvin :: Num a => a
    maxKelvin = 9000
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
