-- TODO remove when `OverloadedRecordUpdate` is fully implemented (hopefully 9.4), and remove type applications
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lifx.Lan.Mock.Terminal (Mock, runMock, MockError) where

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

newtype Mock a = Mock (StateT (Map Device LightState) (ReaderT [Device] (ExceptT MockError IO)) a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadError MockError
        , MonadReader [Device]
        , MonadState (Map Device LightState)
        )

runMock :: [(Device, ByteString)] -> Mock a -> IO (Either MockError a)
runMock ds (Mock x) = do
    mw <- fmap snd <$> getTerminalSize
    for_ ds \(_, d) ->
        BS.putStr $
            d <> BS.replicate (maybe 1 (\w -> (w `div` length ds) - BS.length d) mw) ' '
    putStrLn ""
    runExceptT
        . flip
            runReaderT
            (fst <$> ds)
        . flip
            evalStateT
            (Map.fromList $ ds <&> second (LightState (HSBK 0 0 0 0) 1))
        $ x

data MockError
    = MockNoSuchDevice Device
    | MockProductLookupError ProductLookupError
    deriving (Show)

instance MonadLifx Mock where
    type MonadLifxError Mock = MockError
    lifxThrow = throwError
    liftProductLookupError = MockProductLookupError

    sendMessage d m = do
        s <- maybe (lifxThrow $ MockNoSuchDevice d) pure =<< gets (Map.lookup d)
        r <- case m of
            GetService -> err
            GetHostFirmware -> err
            GetPower -> pure $ StatePower s.power
            SetPower power -> modify $ Map.insert @_ @LightState d s{power = fromIntegral $ fromEnum power}
            GetVersion -> err
            GetColor -> pure s
            SetColor hsbk _t -> modify $ Map.insert d s{hsbk}
            SetLightPower _ _ -> err
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
        pure r
      where
        mkSGR LightState{..} =
            if power /= 0
                then [SetRGBColor Background . uncurryRGB sRGB $ hsbkToRgb hsbk]
                else []
        err = error "message unimplemented" -- TODO
    broadcastMessage m = ask >>= traverse \d -> (d,) <$> sendMessage d m
    discoverDevices x = maybe id take x <$> ask

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
