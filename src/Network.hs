module Network where
{- | Network crap that is static. -}
import Control.Monad.IO.Class
import Data.Binary
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Network.Simple.TCP
import System.IO
import Data.Int

type CallID = Int

type Function = [B.ByteString] -> IO B.ByteString

type ServerState = [(CallID, Function)]

readTCPSocket :: (MonadIO m) => Socket -> m B.ByteString
readTCPSocket socket = do
  -- first 8 bytes (Int64) encodes the msg size
  mSize <- recv socket 8
  let mSize_ = fromMaybe err mSize
  -- read the actual message body now
  msgBody <- recv socket (decode $ B.fromStrict mSize_)
  return $ B.fromStrict $ fromMaybe err msgBody
  where
    err = error "Error parsing request"

lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup a' ((a, b) : as)
  | a == a' = Just b
  | otherwise = Network.lookup a' as

onEvent :: [(Int, [B.ByteString] -> IO B.ByteString)] -> B.ByteString -> Socket -> IO ()
onEvent mapping incoming socket = do
  let (id, args) = decode incoming :: (Int, [B.ByteString])
      Just f = Network.lookup id mapping
  result <- f args
  let res = handleVoidTy result -- we can not send () over wire
  sendLazy socket (B.append (msgSize res) res)
  where
    msgSize r = encode $ B.length r
    handleVoidTy r =
      if (B.length r == 0) -- the () type has msg length 0
        then encode '\0'
        else r

serveForever :: (String, ServiceName) -> [(CallID, Function)] -> IO ()
serveForever (loc, port) vTable = serve (Host loc) port $ \(connSock, remoteAddr) -> do
  putStrLn $ concat ["TCP connection established from ", show remoteAddr]
  hFlush stdout
  req <- readTCPSocket connSock
  onEvent vTable req connSock

createPayload :: (Int, [B.ByteString]) -> B.ByteString
createPayload msg = payload
  where
    msgBody :: B.ByteString
    msgBody = encode msg

    msgSize :: Int64
    msgSize = B.length msgBody

    bytstr :: B.ByteString
    bytstr = encode msgSize

    payload :: B.ByteString
    payload = B.append bytstr msgBody

tryConnect :: Binary a => String -> Int -> (Int, [B.ByteString]) -> IO a
tryConnect ip port (id, args) = connect ip (show port) $ \(socket, remoteaddr) -> do
  putStrLn $ concat ["connection established to ", show remoteaddr]
  sendLazy socket $ createPayload (id, reverse args)
  resp <- readTCPSocket socket
  return $ decode resp