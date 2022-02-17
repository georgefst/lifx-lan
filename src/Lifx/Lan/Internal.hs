module Lifx.Lan.Internal where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Binary.Get
import Data.List
import Data.Tuple.Extra
import Data.Word
import Network.Socket

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import GHC.Generics (Generic)

import Lifx.Internal.ProductInfoMap (ProductLookupError)

-- | A LIFX device, such as a bulb.
newtype Device = Device {unwrap :: HostAddress}
    deriving newtype (Eq, Ord)

instance Show Device where
    show (Device ha) = let (a, b, c, d) = hostAddressToTuple ha in intercalate "." $ map show [a, b, c, d]

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

-- | A monad for sending and receiving LIFX messages.
class MonadIO m => MonadLifxIO m where
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

instance MonadIO m => MonadLifxIO (LifxT m) where
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

{- Util -}

-- | Safe, wraparound variant of 'succ'.
succ' :: (Eq a, Bounded a, Enum a) => a -> a
succ' e
    | e == maxBound = minBound
    | otherwise = succ e
