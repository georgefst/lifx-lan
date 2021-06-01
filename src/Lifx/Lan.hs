module Lifx.Lan where

import Control.Monad
import Data.Binary
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy qualified as BL
import Data.Maybe
import Network.Socket
import Network.Socket.ByteString

{- Usage -}

main :: IO ()
main = setCeilingLight 28000 2300

setCeilingLight :: Word16 -> Word16 -> IO ()
setCeilingLight brightness kelvin = do
    let addr = tupleToHostAddress (192, 168, 1, 190)
        --TODO last 4 digits here must always be 0 - MAC is a Word48
        light = 0xd073d5554f4a0000
        -- day+dusk bulb doesn't support actual colours, so set ignored fields to 0
        colour = HSBK{hue = 0, saturation = 0, ..}
    sendMessage (Just light) addr $ SetColor colour $ Duration 0

{- Core -}

lifxPort :: PortNumber
lifxPort = 56700

--TODO what is the purpose of including a target MAC?
sendMessage :: Maybe Word64 -> HostAddress -> Message -> IO ()
sendMessage light lightAddr msg = do
    --TODO use reader monad to avoid re-binding socket
    sock <- socket AF_INET Datagram defaultProtocol
    bind sock $ SockAddrInet defaultPort 0
    void $ sendTo sock (BL.toStrict $ encodeMessage light msg) (SockAddrInet lifxPort lightAddr)

data HSBK = HSBK
    { hue :: Word16
    , saturation :: Word16
    , brightness :: Word16
    , kelvin :: Word16
    }
newtype Duration = Duration Word32

-- | https://lan.developer.lifx.com/docs/changing-a-device
data Message
    = SetPower Bool
    | SetColor HSBK Duration

{- Low level -}

encodeMessage :: Maybe Word64 -> Message -> BL.ByteString
encodeMessage target msg = runPut $ putHeader (messageHeader target msg) >> putMessagePayload msg

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

messageHeader :: Maybe Word64 -> Message -> Header
messageHeader mtarget = \case
    SetPower{} ->
        Header
            { size = headerSize + 2
            , packetType = 21
            , ..
            }
    SetColor{} ->
        Header
            { size = headerSize + 13 --TODO calculate size of each message?
            , packetType = 102
            , ..
            }
  where
    target = fromMaybe 0 mtarget
    headerSize = 36
    protocol = 1024
    tagged = isNothing mtarget --TODO is this right?
    addressable = True
    origin = 0
    source = 2 --TODO make configurable
    resRequired = False
    ackRequired = False --TODO make configurable (when we have logic for receiving responses)
    sequenceCounter = 1 --TODO increment (requires state monad)

putMessagePayload :: Message -> Put
putMessagePayload = \case
    SetPower b ->
        putWord8 if b then maxBound else minBound
    SetColor HSBK{..} (Duration d) -> do
        putWord8 0
        putWord16le hue
        putWord16le saturation
        putWord16le brightness
        putWord16le kelvin
        putWord32le d
