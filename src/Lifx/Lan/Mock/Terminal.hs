-- TODO remove when `OverloadedRecordUpdate` is fully implemented (and simplify some nested updates) - hopefully 9.4
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Rather than interacting with any bulbs, simulate interactions by printing to a terminal.
module Lifx.Lan.Mock.Terminal (Mock, runMock, MockState (MockState), MockError, runMockFull) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Data.Foldable

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import System.Console.ANSI hiding (SetColor)

import Data.Bifunctor
import Lifx.Internal.Colour
import Lifx.Lan

newtype Mock a = Mock (StateT (Map Device MockState) (ReaderT [Device] (ExceptT MockError IO)) a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadError MockError
        , MonadReader [Device]
        , MonadState (Map Device MockState)
        )

data MockState = MockState
    { light :: LightState
    , service :: Maybe StateService
    , hostFirmware :: Maybe StateHostFirmware
    , version :: Maybe StateVersion
    }

-- TODO this seems like a GHC bug
dotLabel :: LightState -> ByteString
-- dotLabel = (.label)
dotLabel = \LightState{..} -> label

runMock :: [(Device, ByteString)] -> Mock a -> IO (Either MockError a)
runMock = runMockFull . fmap (second \bs -> MockState (LightState (HSBK 0 0 0 0) 1 bs) Nothing Nothing Nothing)

runMockFull :: [(Device, MockState)] -> Mock a -> IO (Either MockError a)
runMockFull ds (Mock x) = do
    mw <- fmap snd <$> getTerminalSize
    for_ ds \(_, s) ->
        let bs = dotLabel s.light
         in BS.putStr $
                bs <> BS.replicate (maybe 1 (\w -> (w `div` length ds) - BS.length bs) mw) ' '
    putStrLn ""
    runExceptT
        . flip
            runReaderT
            (fst <$> ds)
        . flip
            evalStateT
            (Map.fromList ds)
        $ x

data MockError
    = MockNoSuchDevice Device
    | MockProductLookupError ProductLookupError
    | MockDataNotProvided
    deriving (Show)

instance MonadLifx Mock where
    type MonadLifxError Mock = MockError
    lifxThrow = throwError
    liftProductLookupError = MockProductLookupError

    sendMessage d m = do
        s <- maybe (lifxThrow $ MockNoSuchDevice d) pure =<< gets (Map.lookup d)
        r <- case m of
            GetService -> whenProvided s.service
            GetHostFirmware -> whenProvided s.hostFirmware
            GetPower -> pure $ StatePower s.light.power
            SetPower (convertPower -> power) -> modify $ Map.insert d s{light = s.light{power}}
            GetVersion -> whenProvided s.version
            GetColor -> pure s.light
            SetColor hsbk _t -> modify $ Map.insert d s{light = s.light{hsbk}}
            SetLightPower (convertPower -> power) _t -> modify $ Map.insert d s{light = s.light{power}}
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
        whenProvided = maybe (throwError MockDataNotProvided) pure
        convertPower = fromIntegral . fromEnum
        mkSGR s =
            if s.light.power /= 0
                then [SetRGBColor Background . uncurryRGB sRGB $ hsbkToRgb s.light.hsbk]
                else []
    broadcastMessage m = ask >>= traverse \d -> (d,) <$> sendMessage d m
    discoverDevices x = maybe id take x <$> ask
