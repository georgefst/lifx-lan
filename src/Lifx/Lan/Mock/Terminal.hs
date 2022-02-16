module Lifx.Lan.Mock.Terminal (Mock, runMock) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Data.Foldable
import Data.Functor
import Data.Ord
import Data.Tuple.Extra
import Data.Word

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Colour.RGBSpace.HSV (hsv)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import System.Console.ANSI hiding (SetColor)

import Lifx.Lan

newtype Mock a = Mock (StateT (Map Device LightState) (ReaderT [Device] IO) a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader [Device]
        , MonadState (Map Device LightState)
        )

runMock :: [(Device, ByteString)] -> Mock a -> IO a
runMock ds (Mock x) = do
    mw <- fmap snd <$> getTerminalSize
    for_ ds \(_, d) ->
        BS.putStr $
            d <> BS.replicate (maybe 1 (\w -> (w `div` length ds) - BS.length d) mw) ' '
    putStrLn ""
    flip runReaderT (fst <$> ds)
        . flip
            evalStateT
            (Map.fromList $ ds <&> second (LightState (HSBK 0 0 0 0) 1))
        $ x

instance MonadLifx Mock where
    sendMessage d = \case
        GetService -> err
        GetHostFirmware -> err
        GetPower -> getState <&> \LightState{..} -> StatePower{..}
        SetPower b -> do
            modify $ Map.update (pure . \LightState{..} -> LightState{power = fromIntegral $ fromEnum b, ..}) d
            out
        GetVersion -> err
        GetColor -> getState
        SetColor c _t -> do
            modify $ Map.update (pure . \LightState{..} -> LightState{hsbk = c, ..}) d
            out
        SetLightPower _ _ -> err >> out
      where
        err = error "message unimplemented" -- TODO
        getState :: Mock LightState
        getState = maybe (lifxThrow RecvTimeout) pure =<< gets (Map.lookup d)
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
            mkSGR LightState{..} =
                if power /= 0
                    then [SetRGBColor Background . uncurryRGB sRGB $ hsbkToRgb hsbk]
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
