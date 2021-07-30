module Lifx.Lan (
    Device,
    deviceAddress,
    sendMessage,
    broadcastMessage,
    discoverDevices,
    Message (..),
    HSBK (..),
    Lifx,
    runLifx,
    LifxT (..),
    runLifxT,
    LifxError (..),
    MonadLifx (..),

    -- * Responses
    LightState (..),
    StateService (..),
    Service (..),
    StatePower (..),

    -- * Low-level
    deviceFromAddress,
    encodeMessage,
    Header (..),
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Either.Extra
import Data.Fixed
import Data.Foldable
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Data.Tuple.Extra
import Data.Word
import GHC.IO.Exception

import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.Binary.Get (
    ByteOffset,
    Get,
    getByteString,
    getWord16le,
    getWord32le,
    getWord64be,
    getWord8,
    runGetOrFail,
    skip,
 )
import Data.Binary.Put (
    Put,
    putWord16le,
    putWord32le,
    putWord64be,
    putWord8,
    runPut,
 )
import Data.Bits (Bits (..))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Time (
    NominalDiffTime,
    diffUTCTime,
    getCurrentTime,
    nominalDiffTimeToSeconds,
 )
import GHC.Generics (Generic)
import Network.Socket (
    Family (AF_INET),
    HostAddress,
    PortNumber,
    SockAddr (SockAddrInet),
    Socket,
    SocketOption (Broadcast),
    SocketType (Datagram),
    bind,
    defaultPort,
    defaultProtocol,
    hostAddressToTuple,
    setSocketOption,
    socket,
    tupleToHostAddress,
 )
import Network.Socket.ByteString (recvFrom, sendTo)
import System.Random (randomIO)
import System.Timeout (timeout)

{- Device -}

newtype Device = Device {unDevice :: HostAddress}
    deriving newtype (Eq, Ord)
instance Show Device where
    show (Device ha) = let (a, b, c, d) = hostAddressToTuple ha in intercalate "." $ map show [a, b, c, d]

{- |
>>> deviceFromAddress (192, 168, 0, 1)
192.168.0.1

'Device's are really just 'HostAddress's, but you don't need to know that to use this library.
Prefer to get devices from 'discoverDevices' where possible, rather than hardcoding addresses.
-}
deviceFromAddress :: (Word8, Word8, Word8, Word8) -> Device
deviceFromAddress = Device . tupleToHostAddress

deviceAddress :: Device -> HostAddress
deviceAddress = unDevice

{- Core -}

lifxPort :: PortNumber
lifxPort = 56700

sendMessage :: MonadLifx m => Device -> Message a -> m a
sendMessage receiver msg = do
    incrementCounter
    sendMessage' True (unDevice receiver) msg
    Dict <- pure $ msgResWitness msg
    getSendResult receiver

broadcastMessage :: MonadLifx m => Message a -> m [(Device, a)]
broadcastMessage msg =
    msgResWitness msg & \Dict ->
        concatMap (\(a, xs) -> map (a,) $ toList xs) . Map.toList
            <$> broadcastAndGetResult (const $ pure . pure) Nothing msg

{- | If an integer argument is given, wait until we have responses from that number of devices.
Otherwise just keep waiting until timeout.
-}
discoverDevices :: MonadLifx m => Maybe Int -> m [Device]
discoverDevices nDevices = Map.keys <$> broadcastAndGetResult f p GetService
  where
    f _addr StateService{..} = do
        checkPort port
        pure . guard $ service == ServiceUDP
    p = nDevices <&> \n -> (>= n) . length

data HSBK = HSBK
    { hue :: Word16
    , saturation :: Word16
    , brightness :: Word16
    , -- | takes values in the range 1500 to 9000
      kelvin :: Word16
    }
    deriving (Eq, Ord, Show, Generic)

data Message a where
    GetService :: Message StateService
    GetPower :: Message StatePower
    SetPower :: Bool -> Message ()
    GetColor :: Message LightState
    SetColor :: HSBK -> NominalDiffTime -> Message ()
    SetLightPower :: Bool -> NominalDiffTime -> Message ()
deriving instance (Eq (Message a))
deriving instance (Ord (Message a))
deriving instance (Show (Message a))

data Service
    = ServiceUDP
    | ServiceReserved1
    | ServiceReserved2
    | ServiceReserved3
    | ServiceReserved4
    deriving (Eq, Ord, Show, Generic)
data StateService = StateService
    { service :: Service
    , port :: PortNumber
    }
    deriving (Eq, Ord, Show, Generic)
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
    | BroadcastTimeout [HostAddress] -- contains the addresses which we have received valid responses from
    | WrongPacketType Word16 Word16 -- expected, then actual
    | WrongSender Device HostAddress -- expected, then actual
    | UnexpectedSockAddrType SockAddr
    | UnexpectedPort PortNumber
    deriving (Eq, Ord, Show, Generic)

{- Message responses -}

class MessageResult a where
    getSendResult :: MonadLifx m => Device -> m a
    default getSendResult :: (MonadLifx m, Response a) => Device -> m a
    getSendResult receiver = untilJustM do
        timeoutDuration <- getTimeout
        (bs, sender0) <- throwEither $ maybeToEither RecvTimeout <$> receiveMessage timeoutDuration (messageSize @a)
        sender <- hostAddressFromSock sender0
        res <- decodeMessage @a bs
        when (isJust res && sender /= deviceAddress receiver) $ lifxThrow $ WrongSender receiver sender
        pure res
      where
        throwEither x =
            x >>= \case
                Left e -> lifxThrow e
                Right r -> pure r

    broadcastAndGetResult ::
        MonadLifx m =>
        -- | Transform output and discard messages which return 'Nothing'.
        (HostAddress -> a -> m (Maybe b)) ->
        -- | Return once this predicate over received messages passes. Otherwise just keep waiting until timeout.
        Maybe (Map HostAddress (NonEmpty b) -> Bool) ->
        Message a ->
        m (Map Device (NonEmpty b))
    default broadcastAndGetResult ::
        (MonadLifx m, Response a) =>
        (HostAddress -> a -> m (Maybe b)) ->
        Maybe (Map HostAddress (NonEmpty b) -> Bool) ->
        Message a ->
        m (Map Device (NonEmpty b))
    broadcastAndGetResult filter' maybeFinished msg = do
        timeoutDuration <- getTimeout
        incrementCounter
        sendMessage' False (tupleToHostAddress (255, 255, 255, 255)) msg
        t0 <- liftIO getCurrentTime
        fmap (Map.mapKeysMonotonic Device) . flip execStateT Map.empty $ untilM do
            t <- liftIO getCurrentTime
            let timeLeft = timeoutDuration - nominalDiffTimeToInt @Micro (diffUTCTime t t0)
            if timeLeft < 0
                then pure False
                else
                    receiveMessage timeLeft (messageSize @a) >>= \case
                        Just (bs, addr) -> do
                            decodeMessage @a bs >>= \case
                                Just x -> do
                                    hostAddr <- hostAddressFromSock addr
                                    lift (filter' hostAddr x) >>= \case
                                        Just x' -> modify $ Map.insertWith (<>) hostAddr (pure x')
                                        Nothing -> pure ()
                                Nothing -> pure ()
                            maybe (pure False) gets maybeFinished
                        Nothing -> do
                            -- if we were waiting for a predicate to pass, then we've timed out
                            when (isJust maybeFinished) $ lifxThrow . BroadcastTimeout =<< gets Map.keys
                            pure True

class Response a where
    expectedPacketType :: Word16
    messageSize :: Int
    getBody :: Get a

instance MessageResult () where
    getSendResult = const $ pure ()
    broadcastAndGetResult = const . const . const $ pure Map.empty
instance Response StateService where
    expectedPacketType = 3
    messageSize = 5
    getBody = do
        service <-
            getWord8 >>= \case
                1 -> pure ServiceUDP
                2 -> pure ServiceReserved1
                3 -> pure ServiceReserved2
                4 -> pure ServiceReserved3
                5 -> pure ServiceReserved4
                n -> fail $ "unknown service: " <> show n
        port <- fromIntegral <$> getWord32le
        pure StateService{..}
instance MessageResult StateService
instance Response LightState where
    expectedPacketType = 107
    messageSize = 52
    getBody = do
        hsbk <- HSBK <$> getWord16le <*> getWord16le <*> getWord16le <*> getWord16le
        skip 2
        power <- getWord16le
        label <- BS.takeWhile (/= 0) <$> getByteString 32
        skip 8
        pure LightState{..}
instance MessageResult LightState
instance Response StatePower where
    expectedPacketType = 22
    messageSize = 2
    getBody = StatePower <$> getWord16le
instance MessageResult StatePower

-- all `Message` response types are instances of `MessageResult`
--TODO ImpredicativeTypes:
-- msgResWitness :: Message a -> (forall a. MessageResult a => x) -> x
msgResWitness :: Message a -> Dict (MessageResult a)
msgResWitness = \case
    GetService{} -> Dict
    GetPower{} -> Dict
    SetPower{} -> Dict
    GetColor{} -> Dict
    SetColor{} -> Dict
    SetLightPower{} -> Dict
data Dict c where
    Dict :: c => Dict c

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
    liftIO $ setSocketOption sock Broadcast 1
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
instance MonadIO m => MonadLifx (LifxT m) where
    getSocket = LifxT $ asks fst3
    getSource = LifxT $ asks snd3
    getTimeout = LifxT $ asks thd3
    incrementCounter = LifxT $ modify succ'
    getCounter = LifxT $ gets id
    lifxThrow = LifxT . throwError
instance MonadLifx m => MonadLifx (MaybeT m) where
    getSocket = lift getSocket
    getSource = lift getSource
    getTimeout = lift getTimeout
    incrementCounter = lift incrementCounter
    getCounter = lift getCounter
    lifxThrow = lift . lifxThrow
instance MonadLifx m => MonadLifx (ExceptT e m) where
    getSocket = lift getSocket
    getSource = lift getSource
    getTimeout = lift getTimeout
    incrementCounter = lift incrementCounter
    getCounter = lift getCounter
    lifxThrow = lift . lifxThrow
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

encodeMessage :: Bool -> Bool -> Word8 -> Word32 -> Message a -> BL.ByteString
encodeMessage tagged ackRequired sequenceCounter source msg =
    runPut $ Binary.put (messageHeader tagged ackRequired sequenceCounter source msg) >> putMessagePayload msg

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

messageHeader :: Bool -> Bool -> Word8 -> Word32 -> Message a -> Header
messageHeader tagged ackRequired sequenceCounter source = \case
    GetService{} ->
        Header
            { size = headerSize
            , packetType = 2
            , ..
            }
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
    addressable = True
    origin = 0 :: Word8
    resRequired = False

putMessagePayload :: Message a -> Put
putMessagePayload = \case
    GetService -> mempty
    GetPower -> mempty
    SetPower b ->
        putWord16le if b then maxBound else minBound
    GetColor -> mempty
    SetColor HSBK{..} d -> do
        putWord8 0
        putWord16le hue
        putWord16le saturation
        putWord16le brightness
        putWord16le kelvin
        putWord32le $ nominalDiffTimeToInt @Milli d
    SetLightPower b d -> do
        putWord16le if b then maxBound else minBound
        putWord32le $ nominalDiffTimeToInt @Milli d

{- Util -}

-- | Safe, wraparound variant of 'succ'.
succ' :: (Eq a, Bounded a, Enum a) => a -> a
succ' e
    | e == maxBound = minBound
    | otherwise = succ e

headerSize :: Num a => a
headerSize = 36

nominalDiffTimeToInt :: forall f a r. (HasResolution r, f ~ Fixed r, Integral a) => NominalDiffTime -> a
nominalDiffTimeToInt t = fromInteger n
  where
    MkFixed n = realToFrac @Pico @f $ nominalDiffTimeToSeconds t

-- | Inverted 'whileM'.
untilM :: Monad m => m Bool -> m ()
untilM = whileM . fmap not

checkPort :: MonadLifx f => PortNumber -> f ()
checkPort port = when (port /= fromIntegral lifxPort) . lifxThrow $ UnexpectedPort port

-- these helpers are all used by 'sendMessage' and 'broadcastMessage'
decodeMessage :: forall b m. (Response b, MonadLifx m) => BS.ByteString -> m (Maybe b) -- Nothing means counter mismatch
decodeMessage bs = do
    counter <- getCounter
    case runGetOrFail Binary.get $ BL.fromStrict bs of
        Left e -> throwDecodeFailure e
        Right (bs', _, Header{packetType, sequenceCounter}) ->
            if sequenceCounter /= counter
                then handleOldMessage counter sequenceCounter packetType bs' >> pure Nothing
                else do
                    when (packetType /= expectedPacketType @b) . lifxThrow $
                        WrongPacketType (expectedPacketType @b) packetType
                    case runGetOrFail getBody bs' of
                        Left e -> throwDecodeFailure e
                        Right (_, _, res) -> pure $ Just res
  where
    throwDecodeFailure (bs', bo, e) = lifxThrow $ DecodeFailure (BL.toStrict bs') bo e
sendMessage' :: MonadLifx m => Bool -> HostAddress -> Message a -> m ()
sendMessage' tagged receiver msg = do
    sock <- getSocket
    counter <- getCounter
    source <- getSource
    void . liftIO $
        sendTo
            sock
            (BL.toStrict $ encodeMessage tagged False counter source msg)
            (SockAddrInet lifxPort receiver)
hostAddressFromSock :: MonadLifx m => SockAddr -> m HostAddress
hostAddressFromSock = \case
    SockAddrInet port ha -> checkPort port >> pure ha
    addr -> lifxThrow $ UnexpectedSockAddrType addr
receiveMessage :: MonadLifx m => Int -> Int -> m (Maybe (BS.ByteString, SockAddr))
receiveMessage t size = do
    sock <- getSocket
    liftIO
        . timeout t
        . recvFrom sock
        $ headerSize + size
