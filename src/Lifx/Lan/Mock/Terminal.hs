-- TODO remove when `OverloadedRecordUpdate` is fully implemented (and simplify some nested updates) - hopefully 9.4
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Rather than interacting with any bulbs, simulate interactions by printing to a terminal.
module Lifx.Lan.Mock.Terminal (Mock, MockError, runMock, runMockFull, MockState (MockState)) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Traversable
import Data.Tuple.Extra
import Numeric.Natural

import Data.ByteString qualified as BS
import Data.List (unfoldr)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Console.ANSI hiding (SetColor)

import Lifx.Internal.Colour
import Lifx.Lan

newtype Mock a = Mock (StateT (Map Device MockState) (ReaderT [Device] (ExceptT MockError IO)) a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        )

data MockState = MockState
    { light :: LightState
    , group :: StateGroup
    , service :: Maybe StateService
    , hostFirmware :: Maybe StateHostFirmware
    , version :: Maybe StateVersion
    }

dotLabel :: LightState -> Text
-- dotLabel = (.label) -- TODO this is a GHC bug: https://gitlab.haskell.org/ghc/ghc/-/issues/21226
dotLabel LightState{..} = label

{- | Run a LIFX action by mocking effects in a terminal.

Note that sending some messages (e.g. 'GetVersion') will throw exceptions, since the necessary state isn't specified.
See `runMockFull` for more control.
-}
runMock :: [(Device, (Text, Text))] -> Mock a -> IO (Either MockError a)
runMock ds m = do
    t0 <- getPOSIXTime
    flip runMockFull m . flip evalState 0 $ for ds \d ->
        state ((BS.pack . unfoldNat) &&& succ) <&> \uuid ->
            d & second \(label, group) ->
                MockState
                    { light = LightState (HSBK 0 0 0 0) 1 label
                    , group = StateGroup uuid group t0
                    , service = Nothing
                    , hostFirmware = Nothing
                    , version = Nothing
                    }
  where
    -- represent input as base-(maxBound @a)
    unfoldNat :: forall a. (Integral a, Bounded a) => Natural -> [a]
    unfoldNat = unfoldr \n -> guard (n /= 0) $> swap (second fromIntegral (n `quotRem` fromIntegral (maxBound @a)))

-- | More general version of `runMock`, which allows specifying extra information about devices.
runMockFull :: [(Device, MockState)] -> Mock a -> IO (Either MockError a)
runMockFull ds (Mock x) =
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
    lifxThrow = Mock . throwError
    liftProductLookupError = MockProductLookupError

    sendMessage d m = do
        s <- lookupDevice d
        r <- Mock case m of
            GetService -> whenProvided s.service
            GetHostFirmware -> whenProvided s.hostFirmware
            GetPower -> pure $ StatePower s.light.power
            SetPower (convertPower -> power) -> modify $ Map.insert d s{light = s.light{power}}
            SetLabel label -> modify $ Map.insert d s{light = s.light{label}}
            GetVersion -> whenProvided s.version
            GetGroup -> pure s.group
            GetColor -> pure s.light
            SetColor hsbk _t -> modify $ Map.insert d s{light = s.light{hsbk}}
            SetLightPower (convertPower -> power) _t -> modify $ Map.insert d s{light = s.light{power}}
        ds <- Mock ask
        for_ ds \d' -> do
            s' <- lookupDevice d'
            liftIO do
                setSGR $ mkSGR s'.light
                T.putStr $ dotLabel s'.light
                setSGR []
        liftIO $ putStrLn ""
        pure r
      where
        lookupDevice = maybe (lifxThrow $ MockNoSuchDevice d) pure <=< Mock . gets . Map.lookup
        whenProvided = maybe (throwError MockDataNotProvided) pure
        convertPower = fromIntegral . fromEnum
        mkSGR s = [SetRGBColor Background . uncurryRGB sRGB $ hsbkToRgb s.hsbk | s.power /= 0]
    broadcastMessage m = Mock ask >>= traverse \d -> (d,) <$> sendMessage d m
    discoverDevices = Mock . asks . maybe id take
