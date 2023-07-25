{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lifx.Lan (
    main,
) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Data.Binary.Get hiding (label)
import Data.Binary.Put
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Word
import Network.Socket
import Network.Socket.ByteString
import System.Exit (exitSuccess)
import System.Random

main = do
    sock <- liftIO $ socket AF_INET Datagram defaultProtocol
    setSocketOption sock Broadcast 1

    -- bind sock $ SockAddrInet defaultPort 0
    -- bizarrely, this actually works for the overall problem
    -- I suppose the earlier behaviour with repeats was "it works if there's been other recent activity on that port, even from another address"
    -- also weirdly, there's about a 5-second cool-off in GHCI in which using the same address as before doesn't work
    let myPort = 56721
    bind sock $ SockAddrInet myPort 0

    -- void $ sendTo sock "yo" (SockAddrInet myPort (tupleToHostAddress (127, 0, 0, 1)))
    -- print =<< recv sock 4096

    -- const exitSuccess =<< print =<< recv sock 4096

    source <- randomIO
    counter <- randomIO

    threadDelay 500_000
    sendRes <-
        sendTo
            sock
            (BL.toStrict $ runPut $ encodeMessage True False counter source)
            -- (SockAddrInet lifxPort (tupleToHostAddress (192, 168, 1, 190)))
            (SockAddrInet lifxPort (tupleToHostAddress (255, 255, 255, 255)))
    putStrLn $ "sent: " <> show sendRes

    recvRes <- recvFrom sock $ headerSize + lightStateMessageSize
    putStrLn $ "received: " <> show recvRes

    -- pure case runGetOrFail getHeader $ BL.fromStrict $ fst recvRes of
    --     Left e -> error $ show e
    --     Right (bs', _, Header{}) -> case runGetOrFail @LightState getBody bs' of
    --         Left e -> error $ show e
    --         Right (_, _, res) -> case snd recvRes of
    --             SockAddrInet _ ha -> (hostAddressToTuple ha, res)
    --             a -> error $ show a

lifxPort = 56700

encodeMessage tagged ackRequired sequenceCounter source =
    putHeader (messageHeader tagged ackRequired sequenceCounter source) >> getColorPutMessagePayload

getColorPutMessagePayload = mempty
getColorPacketType = 101

data LightState = LightState
    { hsbk :: HSBK
    , power :: Word16
    , label :: Text
    }
    deriving (Eq, Ord, Show)
data HSBK = HSBK
    { hue :: Word16
    , saturation :: Word16
    , brightness :: Word16
    , kelvin :: Word16
    }
    deriving (Eq, Ord, Show)
lightStateMessageSize = 52
getBody = do
    hsbk <- HSBK <$> getWord16le <*> getWord16le <*> getWord16le <*> getWord16le
    skip 2
    power <- getWord16le
    label <- either (fail . showDecodeError) pure . decodeUtf8' . BS.takeWhile (/= 0) =<< getByteString 32
    skip 8
    pure LightState{..}
  where
    showDecodeError = \case
        DecodeError s _ -> s
        _ -> "impossible"

headerSize = 36
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
    deriving (Eq, Ord, Show)
getHeader = do
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
messageHeader tagged ackRequired sequenceCounter source =
    Header
        { size = headerSize
        , packetType = getColorPacketType
        , ..
        }
  where
    target = 0 :: Word64
    protocol = 1024 :: Word16
    addressable = True
    origin = 0 :: Word8
    resRequired = False
