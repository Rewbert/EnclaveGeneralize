{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Client2 where

import Control.Monad.State as ST
import Data.Binary
import Data.ByteString.Lazy (ByteString, append, fromStrict, length)
import qualified Data.Map as Map
import Data.Maybe
import Data.Proxy
import Data.Int
import Network.Simple.TCP

import Debug.Trace

{-

-- maybe JSON configuration file? Can be read and parsed at compile time

{ "endpoints": [ { "name": "Client1"
                 , "role": "client"
                 , "location": {"ip": 127.0.0.1, "port": 8000}
                 }

               , { "name": "Client2"
                 , "role": "client"
                 , "location": {"ip": 127.0.0.1, "port": 8001}
                 }

               , { "name": "Enclave1"
                 , "role": "server"
                 , "location": {"ip": 127.0.0.1, "port": 8002}
                 }

               , { "name": "Enclave2"
                 , "role": "server"
                 , "location": {"ip": 127.0.0.1, "port": 8003}
                 }
               ]
}

-- the above, when indicated that we wish to compile e.g Client1, may generate

-}

newtype Client2 a = Client2 (IO a)
  deriving (Functor) via IO
  deriving (Applicative) via IO
  deriving (Monad) via IO
  deriving (MonadIO) via IO

data Client1 a = Client1 deriving (Functor, Applicative, Monad, MonadIO)

data Enclave1 a = Enclave1 deriving (Functor, Applicative, Monad)

data Enclave2 a = Enclave2 deriving (Functor, Applicative, Monad)

data AppState = AppState {counter :: Int, funs :: Map.Map Int ([ByteString] -> ByteString)}

newtype App a = App (StateT AppState IO a)
  deriving (Functor) via (StateT AppState IO)
  deriving (Applicative) via (StateT AppState IO)
  deriving (Monad) via (StateT AppState IO)
  deriving (MonadState AppState) via (StateT AppState IO)
  deriving (MonadIO) via (StateT AppState IO)

class Securable a where
  mkSecure :: a -> ([ByteString] -> ByteString)

instance (Binary a) => Securable (Enclave1 a) where
  mkSecure _ = \_ -> encode '\0' -- idk what to do here, this is never invoked on the client

instance (Binary a, Securable b) => Securable (a -> b) where
  mkSecure _ = \_ -> encode '\0' -- ditto

class ShouldRun (m :: * -> *) where
  shouldIRun :: proxy m -> Bool
  toIO :: m a -> IO ()

instance ShouldRun Client2 where
  shouldIRun _ = True
  toIO (Client2 ma) = ma >> return ()

instance ShouldRun Client1 where
  shouldIRun _ = False
  toIO _ = return ()

data ClientEndpoint where
  Endpoint :: (ShouldRun m) => m a -> ClientEndpoint

-- * Data families

type family Endpoint (l :: k) :: * -> *

type instance Endpoint "client1" = Client1
type instance Endpoint "client2" = Client2
type instance Endpoint "enclave1" = Enclave1
type instance Endpoint "enclave2" = Enclave2

-- * inEnclave functionality

data Secure a = Secure Int (String, Int) [ByteString]

{-# NOINLINE inEnclave #-}

{-# RULES
"inEnclave1" forall (f :: a). inEnclave @Enclave1 f = inEnclave1 @a
"inEnclave2" forall (f :: a). inEnclave @Enclave2 f = inEnclave2 @a
  #-}

inEnclave :: forall (l :: * -> *) a. (Securable a) => a -> App (Secure a)
inEnclave _ = error "inEnclave applied to unresolved location indicator, please use one of Enclave1 or Enclave2"

inEnclave1 :: forall a. (Securable a) => App (Secure a)
inEnclave1 = do
  st <- ST.get
  ST.put $ st {counter = counter st + 1}
  return $ Secure (counter st) ("127.0.0.1", 8002) []

inEnclave2 :: forall a. (Securable a) => App (Secure a)
inEnclave2 = do
  st <- ST.get
  ST.put $ st {counter = counter st + 1}
  return $ Secure (counter st) ("127.0.0.1", 8003) []

-- * application, should not matter where this is done?

(<@>) :: (Binary a) => Secure (a -> b) -> a -> Secure b
(Secure id loc args) <@> arg = Secure id loc (encode arg : args)

-- * gateway functionality

-- {-# NOINLINE gateway #-}

-- {-# RULES
-- "gatewayClient1" forall (c :: Secure (Enclave1 Bool)) . gateway @Client1 c = gateway1 c
-- "gatewayClient2" forall c . gateway @Client2 c = gateway'
--   #-}

class GateWay m2 where
  gateway :: Binary a => Secure (m a) -> m2 a

instance GateWay Client2 where
  gateway s = gateway1 s

instance GateWay Client1 where
  gateway _ = gateway'

-- gateway :: forall (l :: * -> *) a m m2. (Binary a) => Secure (m a) -> m2 a
-- gateway = error "gateway can not resolve destination, please indicate destination with Client1 or Client2"

gateway1 :: forall a m. (Binary a) => Secure (m a) -> Client2 a
gateway1 (Secure id (ip, port) args) = Client2 $ tryConnect
  where
    tryConnect :: IO (a)
    tryConnect = connect ip (show port) $ \(socket, remoteaddr) -> do
      putStrLn $ concat ["connection established to ", show remoteaddr]
      sendLazy socket $ createPayload (id, reverse args)
      resp <- readTCPSocket socket
      return $ decode resp
--      return $ fmap decode (decode resp :: ByteString)

    createPayload :: (Int, [ByteString]) -> ByteString
    createPayload msg = trace (concat ["length of payload: ", show (Data.ByteString.Lazy.length payload)]) $ payload
      where
        msgBody :: ByteString
        msgBody = encode msg

        msgSize :: Int64
        msgSize = Data.ByteString.Lazy.length msgBody

        bytstr :: ByteString
        bytstr = encode msgSize

        payload :: ByteString
        payload = append bytstr msgBody

    readTCPSocket :: Socket -> IO ByteString
    readTCPSocket socket = do
      -- first 8 bytes (Int64) encodes the msg size
      mSize <- recv socket 8
      let mSize_ = fromMaybe err mSize
      -- read the actual message body now
      msgBody <- recv socket (decode $ fromStrict mSize_)
      return $ Data.ByteString.Lazy.fromStrict $ fromMaybe err msgBody
      where
        err = error "error parsing request"

gateway' :: forall a. Client1 a
gateway' = Client1

-- * Runclient

runClient :: IO a -> IO ()
runClient ma = do
  v <- liftIO ma
  return $ v `seq` ()

runClients :: [ClientEndpoint] -> App ()
runClients [] = return ()
runClients (Endpoint (e :: m a) : xs) = case shouldIRun (Proxy @m) of
  True -> liftIO $ runClient (toIO e)
  False -> runClients xs

runApp :: App a -> IO a
runApp (App s) = evalStateT s (AppState 0 Map.empty)

-- * Reference management