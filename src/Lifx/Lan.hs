module Lifx.Lan (
    sendMessage,
    Message (..),
    HSBK (..),
    Duration (..),
    Lifx,
    runLifx,
    LifxT (..),
    runLifxT,
    LifxError (..),
    MonadLifx (..),

    -- * Responses
    LightState (..),
    StatePower (..),

    -- * Low-level
    encodeMessage,
    Header (..),
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State hiding (get, put)
import Data.Binary
import Data.Binary.Get hiding (label)
import Data.Binary.Put
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Either.Extra
import Data.Function
import Data.Tuple.Extra
import GHC.Generics (Generic)
import GHC.IO.Exception
import Network.Socket
import Network.Socket.ByteString
import System.Random
import System.Timeout

{- Core -}

lifxPort :: PortNumber
lifxPort = 56700

sendMessage :: MonadLifx m => HostAddress -> Message a -> m a
sendMessage lightAddr msg = do
    sock <- getSocket
    source <- getSource
    timeoutDuration <- getTimeout
    counter <- getCounter
    incrementCounter
    let receiver = SockAddrInet lifxPort lightAddr
    void . liftIO $
        sendTo
            sock
            (BL.toStrict $ encodeMessage False counter source msg)
            receiver
    getResponse' msg & either pure \(expectedPacketType, messageSize, getBody) -> do
        (bs, sender) <-
            throwEither . liftIO . fmap (maybeToEither RecvTimeout)
                . timeout timeoutDuration
                . recvFrom sock
                $ headerSize + messageSize
        when (sender /= receiver) $ lifxThrow $ WrongSender receiver sender
        case runGetOrFail get $ BL.fromStrict bs of
            Left e -> throwDecodeFailure e
            Right (bs', _, Header{packetType, sequenceCounter}) -> do
                when (sequenceCounter /= counter) $ lifxThrow $ WrongSequenceNumber counter sequenceCounter
                when (packetType /= expectedPacketType) $ lifxThrow $ WrongPacketType packetType expectedPacketType
                case runGetOrFail getBody bs' of
                    Left e -> throwDecodeFailure e
                    Right (_, _, res) -> pure res
  where
    throwDecodeFailure (bs, bo, e) = lifxThrow $ DecodeFailure (BL.toStrict bs) bo e
    throwEither x =
        x >>= \case
            Left e -> lifxThrow e
            Right r -> pure r

data HSBK = HSBK
    { hue :: Word16
    , saturation :: Word16
    , brightness :: Word16
    , kelvin :: Word16
    }
    deriving (Eq, Ord, Show, Generic)
newtype Duration = Duration Word32
    deriving (Eq, Ord, Show, Generic)

data Message a where
    GetPower :: Message StatePower
    SetPower :: Bool -> Message ()
    GetColor :: Message LightState
    SetColor :: HSBK -> Duration -> Message ()
    SetLightPower :: Bool -> Duration -> Message ()
deriving instance (Eq (Message a))
deriving instance (Ord (Message a))
deriving instance (Show (Message a))

newtype StatePower = StatePower
    { power :: Word16
    }
    deriving (Eq, Ord, Show, Generic)
data LightState = LightState
    { hsbk :: HSBK
    , power :: Word16
    , label :: BS.ByteString
    }
    deriving (Eq, Ord, Show, Generic)

data LifxError
    = DecodeFailure BS.ByteString ByteOffset String
    | RecvTimeout
    | WrongPacketType Word16 Word16 -- expected, then actual
    | WrongSender SockAddr SockAddr -- expected, then actual
    | WrongSequenceNumber Word8 Word8 -- expected, then actual
    deriving (Eq, Ord, Show, Generic)

{- Message responses -}

class Response a where
    getResponse :: Either a (Word16, Int, Get a)

instance Response () where
    getResponse = Left ()
instance Response LightState where
    getResponse = Right $ (107,52,) do
        hsbk <- HSBK <$> getWord16le <*> getWord16le <*> getWord16le <*> getWord16le
        skip 2
        power <- getWord16le
        label <- BS.takeWhile (/= 0) <$> getByteString 32
        skip 8
        pure LightState{..}
instance Response StatePower where
    getResponse =
        Right . (22,2,) $
            StatePower <$> getWord16le

-- | Seeing as all `Message` response types are instances of `Response`, we can hide that type class from users.
getResponse' :: Message a -> Either a (Word16, Int, Get a)
getResponse' = \case
    GetPower{} -> getResponse
    SetPower{} -> getResponse
    GetColor{} -> getResponse
    SetColor{} -> getResponse
    SetLightPower{} -> getResponse

{- Monad -}

type Lifx = LifxT IO
newtype LifxT m a = LifxT
    { unLifxT ::
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

{- | Note that this throws 'LifxError's as 'IOException's, and sets timeout to 5 seconds.
Use 'runLifxT' for more control.
-}
runLifx :: Lifx a -> IO a
runLifx m =
    runLifxT 5_000_000 m >>= \case
        Left e ->
            ioError
                IOError
                    { ioe_handle = Nothing
                    , ioe_type = OtherError
                    , ioe_location = "LIFX"
                    , ioe_description = show e
                    , ioe_errno = Nothing
                    , ioe_filename = Nothing
                    }
        Right x -> pure x

runLifxT :: MonadIO m => Int -> LifxT m a -> m (Either LifxError a)
runLifxT timeoutDuration (LifxT x) = do
    sock <- liftIO $ socket AF_INET Datagram defaultProtocol
    liftIO . bind sock $ SockAddrInet defaultPort 0
    source <- randomIO
    runExceptT $ runReaderT (evalStateT x 0) (sock, source, timeoutDuration)

class MonadIO m => MonadLifx m where
    getSocket :: m Socket
    getSource :: m Word32
    getTimeout :: m Int
    incrementCounter :: m ()
    getCounter :: m Word8
    lifxThrow :: LifxError -> m a
instance MonadIO m => MonadLifx (LifxT m) where
    getSocket = LifxT $ asks fst3
    getSource = LifxT $ asks snd3
    getTimeout = LifxT $ asks thd3
    incrementCounter = LifxT $ modify succ'
    getCounter = LifxT $ gets id
    lifxThrow = LifxT . throwError
instance MonadLifx m => MonadLifx (StateT s m) where
    getSocket = lift getSocket
    getSource = lift getSource
    getTimeout = lift getTimeout
    incrementCounter = lift incrementCounter
    getCounter = lift getCounter
    lifxThrow = lift . lifxThrow
instance MonadLifx m => MonadLifx (ReaderT e m) where
    getSocket = lift getSocket
    getSource = lift getSource
    getTimeout = lift getTimeout
    incrementCounter = lift incrementCounter
    getCounter = lift getCounter
    lifxThrow = lift . lifxThrow

{- Low level -}

encodeMessage :: Bool -> Word8 -> Word32 -> Message a -> BL.ByteString
encodeMessage ackRequired sequenceCounter source msg =
    runPut $ put (messageHeader ackRequired sequenceCounter source msg) >> putMessagePayload msg

-- | https://lan.developer.lifx.com/docs/encoding-a-packet
data Header = Header
    { size :: Word16
    , protocol :: Word16
    , addressable :: Bool
    , tagged :: Bool
    , origin :: Word8
    , source :: Word32
    , target :: Word64
    , resRequired :: Bool
    , ackRequired :: Bool
    , sequenceCounter :: Word8
    , packetType :: Word16
    }
    deriving (Eq, Ord, Show, Generic)

instance Binary Header where
    get = do
        size <- getWord16le
        protBytes <- getWord16le
        let protocol = flip clearBit 12 . flip clearBit 13 . flip clearBit 14 . flip clearBit 15 $ protBytes
            addressable = testBit protBytes 12
            tagged = testBit protBytes 13
            origin = (if testBit protBytes 14 then 0 else 1) + (if testBit protBytes 15 then 0 else 2)
        source <- getWord32le
        target <- getWord64be
        skip 6
        resAckByte <- getWord8
        let resRequired = testBit resAckByte 0
            ackRequired = testBit resAckByte 1
        sequenceCounter <- getWord8
        skip 8
        packetType <- getWord16le
        skip 2
        pure Header{..}

    put Header{..} = do
        putWord16le size
        putWord16le $
            protocol
                .|. bitIf addressable 12
                .|. bitIf tagged 13
                .|. bitIf (testBit origin 0) 14
                .|. bitIf (testBit origin 1) 15
        putWord32le source
        putWord64be target
        replicateM_ 6 $ putWord8 0
        putWord8 $
            zeroBits
                .|. bitIf resRequired 0
                .|. bitIf ackRequired 1
        putWord8 sequenceCounter
        replicateM_ 8 $ putWord8 0
        putWord16le packetType
        replicateM_ 2 $ putWord8 0
      where
        bitIf b n = if b then bit n else zeroBits

messageHeader :: Bool -> Word8 -> Word32 -> Message a -> Header
messageHeader ackRequired sequenceCounter source = \case
    GetPower{} ->
        Header
            { size = headerSize
            , packetType = 20
            , ..
            }
    SetPower{} ->
        Header
            { size = headerSize + 2
            , packetType = 21
            , ..
            }
    GetColor{} ->
        Header
            { size = headerSize
            , packetType = 101
            , ..
            }
    SetColor{} ->
        Header
            { size = headerSize + 13
            , packetType = 102
            , ..
            }
    SetLightPower{} ->
        Header
            { size = headerSize + 6
            , packetType = 117
            , ..
            }
  where
    target = 0 :: Word64
    protocol = 1024 :: Word16
    tagged = True
    addressable = True
    origin = 0 :: Word8
    resRequired = False

putMessagePayload :: Message a -> Put
putMessagePayload = \case
    GetPower -> mempty
    SetPower b ->
        putWord16le if b then maxBound else minBound
    GetColor -> mempty
    SetColor HSBK{..} (Duration d) -> do
        putWord8 0
        putWord16le hue
        putWord16le saturation
        putWord16le brightness
        putWord16le kelvin
        putWord32le d
    SetLightPower b (Duration d) -> do
        putWord16le if b then maxBound else minBound
        putWord32le d

{- Util -}

-- | Safe, wraparound variant of 'succ'.
succ' :: (Eq a, Bounded a, Enum a) => a -> a
succ' e
    | e == maxBound = minBound
    | otherwise = succ e

headerSize :: Num a => a
headerSize = 36
