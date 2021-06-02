module Lifx.Lan where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Binary
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy qualified as BL
import Data.Tuple.Extra
import GHC.Generics (Generic)
import Network.Socket
import Network.Socket.ByteString
import System.Random

{- Core -}

lifxPort :: PortNumber
lifxPort = 56700

sendMessage :: HostAddress -> Message -> Lifx ()
sendMessage lightAddr msg = do
    (sock, source) <- ask
    sequenceCounter <- state $ succ' &&& id
    void . liftIO $
        sendTo
            sock
            (BL.toStrict $ encodeMessage False sequenceCounter source msg)
            (SockAddrInet lifxPort lightAddr)

data HSBK = HSBK
    { hue :: Word16
    , saturation :: Word16
    , brightness :: Word16
    , kelvin :: Word16
    }
    deriving (Eq, Ord, Show, Generic)
newtype Duration = Duration Word32
    deriving (Eq, Ord, Show, Generic)

-- | https://lan.developer.lifx.com/docs/changing-a-device
data Message
    = SetPower Bool
    | SetColor HSBK Duration
    | SetLightPower Bool Duration

{- Monad -}

newtype Lifx a = Lifx {unLifx :: StateT Word8 (ReaderT (Socket, Word32) IO) a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Socket, Word32), MonadState Word8)

runLifx :: Lifx a -> IO a
runLifx (Lifx x) = do
    sock <- socket AF_INET Datagram defaultProtocol
    bind sock $ SockAddrInet defaultPort 0
    source <- randomIO
    runReaderT (evalStateT x 0) (sock, source)

{- Low level -}

encodeMessage :: Bool -> Word8 -> Word32 -> Message -> BL.ByteString
encodeMessage ackRequired sequenceCounter source msg =
    runPut $ putHeader (messageHeader ackRequired sequenceCounter source msg) >> putMessagePayload msg

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

putHeader :: Header -> Put
putHeader Header{..} = do
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

messageHeader :: Bool -> Word8 -> Word32 -> Message -> Header
messageHeader ackRequired sequenceCounter source = \case
    SetPower{} ->
        Header
            { size = headerSize + 2
            , packetType = 21
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
    target = 0
    headerSize = 36
    protocol = 1024
    tagged = True
    addressable = True
    origin = 0
    resRequired = False

putMessagePayload :: Message -> Put
putMessagePayload = \case
    SetPower b ->
        putWord16le if b then maxBound else minBound
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
