{-# LANGUAGE UndecidableInstances #-}

module Lifx.Lan.Internal where

import Control.Exception (Exception (..))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Binary.Get
import Data.List
import Data.Tuple.Extra
import Data.Word
import Network.Socket
import Numeric (showHex)

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import GHC.Generics (Generic)

import Lifx.Internal.ProductInfoMap (ProductLookupError)

-- | A LIFX device, such as a bulb.
newtype Device = Device {unwrap :: HostAddress}
    deriving newtype (Eq, Ord)

instance Show Device where
    show (Device ha) = showHostAddress ha

-- | A colour. See https://lan.developer.lifx.com/docs/representing-color-with-hsbk.
data HSBK = HSBK
    { hue :: Word16
    , saturation :: Word16
    , brightness :: Word16
    , kelvin :: Word16
    -- ^ takes values in the range 1500 to 9000
    }
    deriving (Eq, Ord, Show, Generic)

data LifxError
    = DecodeFailure BS.ByteString ByteOffset String
    | RecvTimeout
    | BroadcastTimeout [HostAddress] -- contains the addresses which we have received valid responses from
    | WrongPacketType Word16 Word16 -- expected, then actual
    | WrongSender Device HostAddress -- expected, then actual
    | UnexpectedSockAddrType SockAddr
    | UnexpectedPort PortNumber
    | ProductLookupError ProductLookupError
    deriving (Eq, Ord, Show, Generic)

instance Exception LifxError where
    displayException = \case
        DecodeFailure bs bo e ->
            "failed to decode response at byte " <> show bo <> ": " <> e <> " (in " <> showBytes bs <> ")"
        RecvTimeout ->
            "timed out waiting for a response from device"
        BroadcastTimeout as ->
            "timed out waiting for responses to a broadcast"
                <> case as of
                    [] -> " (no devices responded)"
                    _ -> " (responses received from: " <> intercalate ", " (map showHostAddress as) <> ")"
        WrongPacketType expected actual ->
            "expected a packet of type " <> show expected <> ", but got one of type " <> show actual
        WrongSender expected actual ->
            "expected a response from " <> show expected <> ", but got one from " <> showHostAddress actual
        UnexpectedSockAddrType addr ->
            "unexpected socket address type: " <> show addr
        UnexpectedPort port ->
            "unexpected port: " <> show port
        ProductLookupError e ->
            "failed to look up product info: " <> displayException e
      where
        showBytes = unwords . map (\w -> let s = showHex w "" in if length s == 1 then '0' : s else s) . BS.unpack

{- | Is this error likely to be transient, such that simply retrying the same operation might succeed?

The LIFX LAN protocol runs over UDP with no delivery guarantees, so dropped packets are a normal
part of operation, rather than a sign that anything is actually wrong. Everything else in
'LifxError' indicates either a misbehaving device or a bug (in this library or in the caller),
and retrying will not help.
-}
isTransient :: LifxError -> Bool
isTransient = \case
    RecvTimeout -> True
    BroadcastTimeout{} -> True
    DecodeFailure{} -> False
    WrongPacketType{} -> False
    WrongSender{} -> False
    UnexpectedSockAddrType{} -> False
    UnexpectedPort{} -> False
    ProductLookupError{} -> False

showHostAddress :: HostAddress -> String
showHostAddress ha = let (a, b, c, d) = hostAddressToTuple ha in intercalate "." $ map show [a, b, c, d]

-- | A monad for sending and receiving LIFX messages.
class (MonadIO m) => MonadLifxIO m where
    getSocket :: m Socket
    getSource :: m Word32
    getTimeout :: m Int
    incrementCounter :: m ()
    getCounter :: m Word8
    lifxThrowIO :: LifxError -> m a
    handleOldMessage ::
        -- | expected counter value
        Word8 ->
        -- | actual counter value
        Word8 ->
        -- | packet type
        Word16 ->
        -- | payload
        BL.ByteString ->
        m ()
    handleOldMessage _ _ _ _ = pure ()

instance (MonadIO m) => MonadLifxIO (LifxT m) where
    getSocket = LifxT $ asks fst3
    getSource = LifxT $ asks snd3
    getTimeout = LifxT $ asks thd3
    incrementCounter = LifxT $ modify succ'
    getCounter = LifxT $ gets id
    lifxThrowIO = LifxT . throwError

newtype LifxT m a = LifxT
    { unwrap ::
        StateT
            Word8
            ( ReaderT
                (Socket, Word32, Int)
                ( ExceptT
                    LifxError
                    m
                )
            )
            a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        )

instance MonadTrans LifxT where
    lift = LifxT . lift . lift . lift
instance (MonadReader s m) => MonadReader s (LifxT m) where
    ask = lift ask
    local f m = LifxT $ StateT \s -> ReaderT \e ->
        ExceptT $ local f $ unLifx e s m
instance (MonadState s m) => MonadState s (LifxT m) where
    state = lift . state
instance (MonadError e m) => MonadError (Either e LifxError) (LifxT m) where
    throwError = either (lift . throwError @e @m) (LifxT . throwError)
    catchError m h = LifxT $ StateT \s -> ReaderT \e -> ExceptT do
        (m', s'') <- either ((,s) . h . Right) (first pure) <$> unLifx e s m
        catchError @e @m (unLifx e s'' m') (unLifx e s'' . h . Left)

unLifx :: (Socket, Word32, Int) -> Word8 -> LifxT m a -> m (Either LifxError (a, Word8))
unLifx e s = runExceptT . flip runReaderT e . flip runStateT s . (.unwrap)

{- Util -}

-- | Safe, wraparound variant of 'succ'.
succ' :: (Eq a, Bounded a, Enum a) => a -> a
succ' e
    | e == maxBound = minBound
    | otherwise = succ e
