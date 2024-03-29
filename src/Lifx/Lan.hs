{- |
@
-- these should be enabled by default in a future version of GHC
-- (they aren't entirely necessary here anyway - they just make the example even simpler)
\{\-\# LANGUAGE BlockArguments \#\-\}
\{\-\# LANGUAGE NamedFieldPuns \#\-\}

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)

import Lifx.Lan

-- | Find all devices on the network, print their addresses, and set their brightness to 50%.
main :: IO ()
main = runLifx do
    devs <- discoverDevices Nothing
    liftIO $ print devs
    for_ devs \\d -> do
        LightState{hsbk} <- sendMessage d GetColor
        sendMessage d $ SetColor hsbk{brightness = maxBound \`div\` 2} 3
@
-}
module Lifx.Lan (
    Device,
    deviceAddress,
    deviceFromAddress,
    Message (..),
    HSBK (..),
    Lifx,
    runLifx,
    LifxT,
    runLifxT,
    LifxError (..),
    ProductLookupError (..),
    MonadLifx (..),
    sendMessageAndWait,

    -- * Responses
    StateService (..),
    Service (..),
    StateHostFirmware (..),
    StatePower (..),
    StateVersion (..),
    StateGroup (..),
    LightState (..),

    -- ** Product info
    getProductInfo,
    Product (..),
    Features (..),

    -- * Message encoding

    -- | These are used internally by `LifxT`'s 'sendMessage' and 'broadcastMessage'.
    -- They are exposed in order to support some advanced use cases.
    encodeMessage,
    Header (..),
) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Composition
import Data.Either.Extra
import Data.Fixed
import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import Network.Socket
import System.IO.Error

import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.Binary.Get (
    Get,
    getByteString,
    getWord16le,
    getWord32le,
    getWord64be,
    getWord64le,
    getWord8,
    runGetOrFail,
    skip,
 )
import Data.Binary.Put (
    Put,
    putByteString,
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
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Text.Encoding.Error (UnicodeException (DecodeError))
import GHC.Generics (Generic)
import Network.Socket.ByteString (recvFrom, sendTo)
import System.Random (randomIO)
import System.Timeout (timeout)

import Lifx.Internal.Product
import Lifx.Internal.ProductInfoMap
import Lifx.Lan.Internal

{- Device -}

{- |
>>> deviceFromAddress (192, 168, 0, 1)
192.168.0.1

If we know the IP address of a `Device`, we can create it directly, rather than calling `discoverDevices`.
-}
deviceFromAddress :: (Word8, Word8, Word8, Word8) -> Device
deviceFromAddress = Device . tupleToHostAddress

deviceAddress :: Device -> HostAddress
deviceAddress = (.unwrap)

{- Core -}

lifxPort :: PortNumber
lifxPort = 56700

-- | A message we can send to a 'Device'. 'r' is the type of the expected response.
data Message r where
    -- | https://lan.developer.lifx.com/docs/querying-the-device-for-data#getservice---packet-2
    -- (you shouldn't need this - use 'discoverDevices')
    GetService :: Message StateService
    -- | https://lan.developer.lifx.com/docs/querying-the-device-for-data#gethostfirmware---packet-14
    GetHostFirmware :: Message StateHostFirmware
    -- | https://lan.developer.lifx.com/docs/querying-the-device-for-data#getpower---packet-20
    GetPower :: Message StatePower
    -- | https://lan.developer.lifx.com/docs/changing-a-device#setpower---packet-21
    SetPower :: Bool -> Message ()
    -- | https://lan.developer.lifx.com/docs/changing-a-device#setlabel---packet-24
    SetLabel :: Text -> Message ()
    -- | https://lan.developer.lifx.com/docs/querying-the-device-for-data#getversion---packet-32
    GetVersion :: Message StateVersion
    -- | https://lan.developer.lifx.com/docs/querying-the-device-for-data#getgroup---packet-51
    GetGroup :: Message StateGroup
    -- | https://lan.developer.lifx.com/docs/querying-the-device-for-data#getcolor---packet-101
    GetColor :: Message LightState
    -- | https://lan.developer.lifx.com/docs/changing-a-device#setcolor---packet-102
    SetColor :: HSBK -> NominalDiffTime -> Message ()
    -- | https://lan.developer.lifx.com/docs/changing-a-device#setlightpower---packet-117
    SetLightPower :: Bool -> NominalDiffTime -> Message ()

deriving instance (Eq (Message r))
deriving instance (Ord (Message r))
deriving instance (Show (Message r))

-- | https://lan.developer.lifx.com/docs/field-types#services
data Service
    = ServiceUDP
    | ServiceReserved1
    | ServiceReserved2
    | ServiceReserved3
    | ServiceReserved4
    deriving (Eq, Ord, Show, Generic)

-- | https://lan.developer.lifx.com/docs/information-messages#stateservice---packet-3
data StateService = StateService
    { service :: Service
    , port :: PortNumber
    }
    deriving (Eq, Ord, Show, Generic)

-- | https://lan.developer.lifx.com/docs/information-messages#statehostfirmware---packet-15
data StateHostFirmware = StateHostFirmware
    { build :: Word64
    -- ^ The timestamp of the firmware that is on the device as an epoch
    , versionMinor :: Word16
    -- ^ The minor component of the firmware version
    , versionMajor :: Word16
    -- ^ The major component of the firmware version
    }
    deriving (Eq, Ord, Show, Generic)

-- | https://lan.developer.lifx.com/docs/information-messages#statepower---packet-22
newtype StatePower = StatePower
    { power :: Word16
    }
    deriving (Eq, Ord, Show, Generic)

-- | https://lan.developer.lifx.com/docs/information-messages#stateversion---packet-33
data StateVersion = StateVersion
    { vendor :: Word32
    -- ^ For LIFX products this value is 1. There may be devices in the future with a different vendor value.
    , product :: Word32
    -- ^ The product id of the device. The available products can be found in our Product Registry.
    }
    deriving (Eq, Ord, Show, Generic)

-- | https://lan.developer.lifx.com/docs/information-messages#stategroup---packet-53
data StateGroup = StateGroup
    { group :: BS.ByteString
    -- ^ The unique identifier of this group as a UUID.
    , label :: Text
    -- ^ The name assigned to this group.
    , updatedAt :: POSIXTime
    -- ^ When this group was set on the device.
    }
    deriving (Eq, Ord, Show, Generic)

-- | https://lan.developer.lifx.com/docs/information-messages#lightstate---packet-107
data LightState = LightState
    { hsbk :: HSBK
    , power :: Word16
    , label :: Text
    }
    deriving (Eq, Ord, Show, Generic)

{- Message responses -}

class MessageResult a where
    getSendResult :: (MonadLifxIO m) => Device -> m a
    default getSendResult :: (MonadLifxIO m, Response a) => Device -> m a
    getSendResult receiver = untilJustM do
        timeoutDuration <- getTimeout
        (bs, sender0) <- throwEither $ maybeToEither RecvTimeout <$> receiveMessage timeoutDuration (messageSize @a)
        sender <- hostAddressFromSock sender0
        res <- decodeMessage @a bs
        when (isJust res && sender /= deviceAddress receiver) $ lifxThrowIO $ WrongSender receiver sender
        pure res
      where
        throwEither x =
            x >>= \case
                Left e -> lifxThrowIO e
                Right r -> pure r

    broadcastAndGetResult ::
        (MonadLifxIO m) =>
        -- | Transform output and discard messages which return 'Nothing'.
        (HostAddress -> a -> m (Maybe b)) ->
        -- | Return once this predicate over received messages passes. Otherwise just keep waiting until timeout.
        Maybe (Map HostAddress (NonEmpty b) -> Bool) ->
        Message r ->
        m (Map Device (NonEmpty b))
    default broadcastAndGetResult ::
        (MonadLifxIO m, Response a) =>
        (HostAddress -> a -> m (Maybe b)) ->
        Maybe (Map HostAddress (NonEmpty b) -> Bool) ->
        Message r ->
        m (Map Device (NonEmpty b))
    broadcastAndGetResult filter' maybeFinished msg = do
        timeoutDuration <- getTimeout
        broadcast msg
        t0 <- liftIO getCurrentTime
        fmap (Map.mapKeysMonotonic Device) . flip execStateT Map.empty . untilM $
            traverse gets maybeFinished >>= \case
                Just True -> pure True
                _ -> do
                    t <- liftIO getCurrentTime
                    let timeLeft = timeoutDuration - nominalDiffTimeToInt @Micro (diffUTCTime t t0)
                    if timeLeft < 0
                        then pure False
                        else
                            lift (receiveMessage timeLeft (messageSize @a)) >>= \case
                                Just (bs, addr) -> do
                                    lift (decodeMessage @a bs) >>= \case
                                        Just x -> do
                                            hostAddr <- lift $ hostAddressFromSock addr
                                            lift (filter' hostAddr x) >>= \case
                                                Just x' -> modify (Map.insertWith (<>) hostAddr (pure x')) >> pure False
                                                Nothing -> pure False
                                        Nothing -> pure False
                                Nothing -> do
                                    -- if we were waiting for a predicate to pass, then we've timed out
                                    when (isJust maybeFinished) $
                                        lift . lifxThrowIO . BroadcastTimeout
                                            =<< gets Map.keys
                                    pure True

class Response a where
    expectedPacketType :: Word16
    messageSize :: Int
    getBody :: Get a

instance MessageResult () where
    getSendResult = const $ pure ()
    broadcastAndGetResult = const $ const $ (Map.empty <$) . broadcast
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
        port <- do
            x <- getWord32le
            -- `network` lib uses `Word16` for ports, but LIFX StateService uses `Word32`
            maybe (fail $ "port out of range: " <> show x) pure $ fromIntegralSafe x
        pure StateService{..}
instance MessageResult StateService
instance Response StateHostFirmware where
    expectedPacketType = 15
    messageSize = 20
    getBody = do
        build <- getWord64le
        skip 8
        versionMinor <- getWord16le
        versionMajor <- getWord16le
        pure StateHostFirmware{..}
instance MessageResult StateHostFirmware
instance Response StatePower where
    expectedPacketType = 22
    messageSize = 2
    getBody = StatePower <$> getWord16le
instance MessageResult StatePower
instance Response StateVersion where
    expectedPacketType = 33
    messageSize = 20
    getBody = do
        vendor <- getWord32le
        p <- getWord32le
        skip 4
        pure StateVersion{product = p, ..}
instance MessageResult StateVersion
instance Response StateGroup where
    expectedPacketType = 53
    messageSize = 56
    getBody = do
        group <- getByteString 16
        label <- either (fail . showDecodeError) pure . decodeUtf8' . BS.takeWhile (/= 0) =<< getByteString 32
        updatedAt <- secondsToNominalDiffTime . MkFixed . (* 1000) . fromIntegral <$> getWord64le
        pure StateGroup{..}
instance MessageResult StateGroup
instance Response LightState where
    expectedPacketType = 107
    messageSize = 52
    getBody = do
        hsbk <- HSBK <$> getWord16le <*> getWord16le <*> getWord16le <*> getWord16le
        skip 2
        power <- getWord16le
        label <- either (fail . showDecodeError) pure . decodeUtf8' . BS.takeWhile (/= 0) =<< getByteString 32
        skip 8
        pure LightState{..}
instance MessageResult LightState

msgResWitness :: ((MessageResult r) => Message r -> a) -> (Message r -> a)
msgResWitness f m = case m of
    GetService{} -> f m
    GetHostFirmware{} -> f m
    GetPower{} -> f m
    SetPower{} -> f m
    SetLabel{} -> f m
    GetVersion{} -> f m
    GetGroup{} -> f m
    GetColor{} -> f m
    SetColor{} -> f m
    SetLightPower{} -> f m

{- Monad -}

-- | A simple implementation of 'MonadLifx'.
type Lifx = LifxT IO

{- | Note that this throws 'LifxError's as 'IOException's, and sets timeout to 5 seconds.
Use 'runLifxT' for more control.
-}
runLifx :: Lifx a -> IO a
runLifx m =
    runLifxT 5_000_000 m >>= \case
        Left e -> ioError $ mkIOError userErrorType ("LIFX LAN: " <> show e) Nothing Nothing
        Right x -> pure x

runLifxT ::
    (MonadIO m) =>
    -- | Timeout for waiting for message responses, in microseconds.
    Int ->
    LifxT m a ->
    m (Either LifxError a)
runLifxT timeoutDuration (LifxT x) = do
    sock <- liftIO $ socket AF_INET Datagram defaultProtocol
    liftIO $ setSocketOption sock Broadcast 1
    liftIO . bind sock $ SockAddrInet defaultPort 0
    source <-
        untilJustM $
            randomIO <&> \case
                -- 0 and 1 cause problems on old firmware: https://lan.developer.lifx.com/docs/packet-contents#frame-header
                n | n > 1 -> Just n
                _ -> Nothing
    runExceptT $ runReaderT (evalStateT x 0) (sock, source, timeoutDuration)

class (Monad m) => MonadLifx m where
    -- | The type of errors associated with 'm'.
    type MonadLifxError m

    liftProductLookupError :: ProductLookupError -> MonadLifxError m
    lifxThrow :: MonadLifxError m -> m a

    -- | Send a message and wait for a response.
    sendMessage :: Device -> Message r -> m r

    -- | Broadcast a message and wait for responses.
    broadcastMessage :: Message r -> m [(Device, r)]

    -- | Search for devices on the local network.
    -- If an integer argument is given, wait until we have found that number of devices -
    -- otherwise just keep waiting until timeout.
    discoverDevices :: Maybe Int -> m [Device]

instance (MonadIO m) => MonadLifx (LifxT m) where
    type MonadLifxError (LifxT m) = LifxError
    lifxThrow = lifxThrowIO
    liftProductLookupError = ProductLookupError

    sendMessage receiver = msgResWitness \msg -> do
        incrementCounter
        sendMessage' True receiver.unwrap msg
        getSendResult receiver

    broadcastMessage =
        msgResWitness $
            fmap (concatMap (\(a, xs) -> map (a,) $ toList xs) . Map.toList)
                . broadcastAndGetResult (const $ pure . pure) Nothing

    discoverDevices nDevices = Map.keys <$> broadcastAndGetResult f p GetService
      where
        f _addr StateService{..} = do
            checkPort port
            pure . guard $ service == ServiceUDP
        p = nDevices <&> \n -> (>= n) . length
instance (MonadLifx m) => MonadLifx (MaybeT m) where
    type MonadLifxError (MaybeT m) = MonadLifxError m
    liftProductLookupError = liftProductLookupError @m
    sendMessage = lift .: sendMessage
    broadcastMessage = lift . broadcastMessage
    discoverDevices = lift . discoverDevices
    lifxThrow = lift . lifxThrow
instance (MonadLifx m) => MonadLifx (ExceptT e m) where
    type MonadLifxError (ExceptT e m) = MonadLifxError m
    liftProductLookupError = liftProductLookupError @m
    sendMessage = lift .: sendMessage
    broadcastMessage = lift . broadcastMessage
    discoverDevices = lift . discoverDevices
    lifxThrow = lift . lifxThrow
instance (MonadLifx m) => MonadLifx (StateT s m) where
    type MonadLifxError (StateT s m) = MonadLifxError m
    liftProductLookupError = liftProductLookupError @m
    sendMessage = lift .: sendMessage
    broadcastMessage = lift . broadcastMessage
    discoverDevices = lift . discoverDevices
    lifxThrow = lift . lifxThrow
instance (MonadLifx m, Monoid t) => MonadLifx (WriterT t m) where
    type MonadLifxError (WriterT t m) = MonadLifxError m
    liftProductLookupError = liftProductLookupError @m
    sendMessage = lift .: sendMessage
    broadcastMessage = lift . broadcastMessage
    discoverDevices = lift . discoverDevices
    lifxThrow = lift . lifxThrow
instance (MonadLifx m) => MonadLifx (ReaderT e m) where
    type MonadLifxError (ReaderT e m) = MonadLifxError m
    liftProductLookupError = liftProductLookupError @m
    sendMessage = lift .: sendMessage
    broadcastMessage = lift . broadcastMessage
    discoverDevices = lift . discoverDevices
    lifxThrow = lift . lifxThrow

encodeMessage ::
    -- | tagged
    Bool ->
    -- | ackRequired
    Bool ->
    -- | sequenceCounter
    Word8 ->
    -- | source
    Word32 ->
    Message r ->
    BL.ByteString
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

messageHeader :: Bool -> Bool -> Word8 -> Word32 -> Message r -> Header
messageHeader tagged ackRequired sequenceCounter source = \case
    GetService{} ->
        Header
            { size = headerSize
            , packetType = 2
            , ..
            }
    GetHostFirmware{} ->
        Header
            { size = headerSize
            , packetType = 14
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
    SetLabel{} ->
        Header
            { size = headerSize + 32
            , packetType = 24
            , ..
            }
    GetVersion{} ->
        Header
            { size = headerSize
            , packetType = 32
            , ..
            }
    GetGroup{} ->
        Header
            { size = headerSize
            , packetType = 51
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

putMessagePayload :: Message r -> Put
putMessagePayload = \case
    GetService -> mempty
    GetHostFirmware -> mempty
    GetPower -> mempty
    SetPower b ->
        putWord16le if b then maxBound else minBound
    SetLabel t -> do
        putByteString b
        replicateM_ (BS.length b) $ putWord8 0
      where
        b = encodeUtf8 t
    GetVersion -> mempty
    GetGroup -> mempty
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

-- | Ask a device for its vendor and product ID, and look up info on it from the official database.
getProductInfo :: forall m. (MonadLifx m) => Device -> m Product
getProductInfo dev = do
    StateHostFirmware{..} <- sendMessage dev GetHostFirmware
    v <- sendMessage dev GetVersion
    either (lifxThrow . liftProductLookupError @m) pure $ productLookup v.vendor v.product versionMinor versionMajor

{- Higher-level helpers -}

{- | Like `sendMessage`, but for messages whose effect is not instantaneous (e.g. `SetColor`),
block (using `threadDelay`) until completion.
-}
sendMessageAndWait :: (MonadLifx m, MonadIO m) => Device -> Message () -> m ()
sendMessageAndWait d m = do
    sendMessage d m
    maybe (pure ()) (liftIO . threadDelay . timeMicros) mt
  where
    mt = case m of
        SetPower{} -> Nothing
        SetLabel{} -> Nothing
        SetColor _ t -> Just t
        SetLightPower _ t -> Just t
    timeMicros t = round $ t * 1_000_000

{- Util -}

fromIntegralSafe :: forall a b. (Integral a, Integral b, Bounded b) => a -> Maybe b
fromIntegralSafe x =
    guard
        ( x <= fromIntegral (maxBound @b)
            && x >= fromIntegral (minBound @b)
        )
        $> fromIntegral x

headerSize :: (Num a) => a
headerSize = 36

nominalDiffTimeToInt :: forall f a r. (HasResolution r, f ~ Fixed r, Integral a) => NominalDiffTime -> a
nominalDiffTimeToInt t = fromInteger n
  where
    MkFixed n = realToFrac @Pico @f $ nominalDiffTimeToSeconds t

-- | Inverted 'whileM'.
untilM :: (Monad m) => m Bool -> m ()
untilM = whileM . fmap not

checkPort :: (MonadLifxIO f) => PortNumber -> f ()
checkPort port = when (port /= lifxPort) . lifxThrowIO $ UnexpectedPort port

-- these helpers are all used by 'sendMessage' and 'broadcastMessage'
decodeMessage :: forall b m. (Response b, MonadLifxIO m) => BS.ByteString -> m (Maybe b) -- Nothing means counter mismatch
decodeMessage bs = do
    counter <- getCounter
    case runGetOrFail Binary.get $ BL.fromStrict bs of
        Left e -> throwDecodeFailure e
        Right (bs', _, Header{packetType, sequenceCounter}) ->
            if sequenceCounter /= counter
                then handleOldMessage counter sequenceCounter packetType bs' >> pure Nothing
                else do
                    when (packetType /= expectedPacketType @b) . lifxThrowIO $
                        WrongPacketType (expectedPacketType @b) packetType
                    case runGetOrFail getBody bs' of
                        Left e -> throwDecodeFailure e
                        Right (_, _, res) -> pure $ Just res
  where
    throwDecodeFailure (bs', bo, e) = lifxThrowIO $ DecodeFailure (BL.toStrict bs') bo e
sendMessage' :: (MonadLifxIO m) => Bool -> HostAddress -> Message r -> m ()
sendMessage' tagged receiver msg = do
    sock <- getSocket
    counter <- getCounter
    source <- getSource
    void . liftIO $
        sendTo
            sock
            (BL.toStrict $ encodeMessage tagged False counter source msg)
            (SockAddrInet lifxPort receiver)
hostAddressFromSock :: (MonadLifxIO m) => SockAddr -> m HostAddress
hostAddressFromSock = \case
    SockAddrInet port ha -> checkPort port >> pure ha
    addr -> lifxThrowIO $ UnexpectedSockAddrType addr
receiveMessage :: (MonadLifxIO m) => Int -> Int -> m (Maybe (BS.ByteString, SockAddr))
receiveMessage t size = do
    sock <- getSocket
    liftIO
        . timeout t
        . recvFrom sock
        $ headerSize + size

broadcast :: (MonadLifxIO m) => Message r -> m ()
broadcast msg = do
    incrementCounter
    sendMessage' False (tupleToHostAddress (255, 255, 255, 255)) msg

showDecodeError :: UnicodeException -> String
showDecodeError = \case
    DecodeError s _ -> s
    _ -> "impossible"
