{-# LANGUAGE UndecidableInstances #-}

module Lifx.Lan.Internal where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Binary.Get
import Data.IORef
import Data.List
import Data.Time (NominalDiffTime)
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
    UnexpectedSockAddrType{} -> False
    UnexpectedPort{} -> False
    ProductLookupError{} -> False

showHostAddress :: HostAddress -> String
showHostAddress ha = let (a, b, c, d) = hostAddressToTuple ha in intercalate "." $ map show [a, b, c, d]

{- | Options for 'Lifx.Lan.runLifxT'. Use 'defaultLifxConfig' and override the fields you care about.

Note that the two timeouts mean quite different things, and belong at quite different orders of
magnitude - see each field.
-}
data LifxConfig = LifxConfig
    { messageTimeout :: NominalDiffTime
    -- ^ How long to wait for a response to a message sent to a particular device, before giving
    -- up. This is a deadline on a failure: in normal operation it never elapses, so it wants to be
    -- not much more than the round trip time to a device on your network.
    , broadcastTimeout :: NominalDiffTime
    -- ^ How long to spend collecting responses to a broadcast (see 'Lifx.Lan.broadcastMessage' and
    -- 'Lifx.Lan.discoverDevices'). Unlike 'messageTimeout' this is not a failure deadline but a
    -- dwell time: since there is no way to know how many devices are out there, we always wait the
    -- full duration. Setting it too low doesn't cause an error, it just silently finds fewer
    -- devices, so it wants to be comfortably longer than 'messageTimeout'.
    , port :: Maybe PortNumber
    -- ^ A port on which to receive messages. This could be useful when using a firewall which
    -- blocks most ports. 'Nothing' uses 'defaultPort'.
    , retries :: Word
    -- ^ How many times to retry an operation which fails with a transient error - see
    -- 'isTransient'. Note that each attempt can take up to the relevant timeout, so the worst case
    -- for an unresponsive device is @(retries + 1) * messageTimeout@.
    }
    deriving (Eq, Ord, Show, Generic)

defaultLifxConfig :: LifxConfig
defaultLifxConfig =
    LifxConfig
        { messageTimeout = 1
        , broadcastTimeout = 2
        , port = Nothing
        , retries = 2
        }

{- | Retry an action for as long as it fails with a transient error, up to @retries@ extra attempts.

Note that we deliberately don't sleep between attempts: for the errors which are worth retrying at
all, we have by definition just spent a whole @timeout@ waiting.
-}
retryingTransient :: (MonadLifxIO m) => m a -> m a
retryingTransient x = getConfig >>= go . (.retries)
  where
    go n = x `catch` \e -> if n > 0 && isTransient e then go (n - 1) else throwM e

-- | A monad for sending and receiving LIFX messages.
class (MonadIO m, MonadCatch m) => MonadLifxIO m where
    getSocket :: m Socket
    getSource :: m Word32
    getConfig :: m LifxConfig
    incrementCounter :: m ()
    getCounter :: m Word8
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

instance (MonadIO m, MonadCatch m) => MonadLifxIO (LifxT m) where
    getSocket = LifxT $ asks (.socket)
    getSource = LifxT $ asks (.source)
    getConfig = LifxT $ asks (.config)
    getCounter = LifxT $ asks (.counter) >>= liftIO . readIORef
    incrementCounter = LifxT $ asks (.counter) >>= liftIO . flip modifyIORef' succ'

data LifxEnv = LifxEnv
    { socket :: Socket
    , source :: Word32
    , config :: LifxConfig
    , counter :: IORef Word8
    -- ^ Deliberately mutable, rather than a 'StateT': if a send throws, we must not roll the
    -- counter back, or the next message would reuse a sequence number that a late reply to the
    -- failed one could still match.
    }

{- | The concrete implementation of 'MonadLifx'.

Note that this is a plain 'ReaderT', so it commutes with everything, and 'LifxError's are thrown
as exceptions rather than living in a dedicated error channel. This means that a @LifxT@ layer no
longer interferes with the caller's own 'MonadError'/'MonadState' instances.
-}
newtype LifxT m a = LifxT {unwrap :: ReaderT LifxEnv m a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadFail
        , MonadThrow
        , MonadCatch
        , MonadMask
        )

deriving newtype instance (MonadState s m) => MonadState s (LifxT m)
deriving newtype instance (MonadError e m) => MonadError e (LifxT m)

instance MonadTrans LifxT where
    lift = LifxT . lift

-- | Note that this passes through to @m@ - it does not expose the internal 'LifxEnv'.
instance (MonadReader s m) => MonadReader s (LifxT m) where
    ask = lift ask
    local f (LifxT (ReaderT g)) = LifxT $ ReaderT $ local f . g

runLifxEnv :: LifxEnv -> LifxT m a -> m a
runLifxEnv e = flip runReaderT e . (.unwrap)

{- Util -}

-- | Safe, wraparound variant of 'succ'.
succ' :: (Eq a, Bounded a, Enum a) => a -> a
succ' e
    | e == maxBound = minBound
    | otherwise = succ e
