{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE TemplateHaskell #-}
module Client1 where

import Control.Monad.State as ST
import Data.Binary
import Data.ByteString.Lazy (ByteString, append, fromStrict, length)
import qualified Data.Map as Map
import Data.Maybe
import Data.Proxy
import Data.Int
import Network.Simple.TCP

import Debug.Trace
import App

import Network
import qualified GenModule as GM
import GenModule hiding (Endpoint)
import qualified Data.ByteString.Lazy as B

-- $(return $ GM.createNewtype GM.current : GM.concreteMonadStack GM.current ++ closures <> [GM.clientShouldRun GM.current] ++ GM.correctGateway GM.current)
-- $(return $ GM.createDummyType GM.current : GM.dummyMonadStack GM.current ++ [GM.clientShouldNotRun GM.current] ++ GM.dummyGateway GM.current ++ GM.secure ++ GM.correctRunClient ++ GM.correctRunClients)
-- $(return $ GM.closures <> GM.dummySecurable)

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

-- * Client1

newtype Client1 a = Client1 (IO a)
  deriving (Functor) via IO
  deriving (Applicative) via IO
  deriving (Monad) via IO
  deriving (MonadIO) via IO

instance ShouldRun Client1 where
  shouldIRun _ = True
  toIO (Client1 ma) = ma >> return ()

type instance Endpoint "client1" = Client1

instance GateWay Client1 where
  gateway s = gateway1 s

-- * Client2 etc

data Client2 a = Client2 deriving (Functor, Applicative, Monad, MonadIO)

data Enclave1 a = Enclave1 deriving (Functor, Applicative, Monad)

data Enclave2 a = Enclave2 deriving (Functor, Applicative, Monad)

class Securable a where
  mkSecure :: a -> ([ByteString] -> ByteString)

instance (Binary a) => Securable (Enclave1 a) where
  mkSecure _ = \_ -> encode '\0' -- idk what to do here, this is never invoked on the client

instance (Binary a) => Securable (Enclave2 a) where
  mkSecure _ = \_ -> encode '\0' -- idk what to do here, this is never invoked on the client

instance (Binary a, Securable b) => Securable (a -> b) where
  mkSecure _ = \_ -> encode '\0' -- ditto


instance ShouldRun Client2 where
  shouldIRun _ = False
  toIO _ = return ()

-- * Data families

type instance Endpoint "client2" = Client2
type instance Endpoint "enclave1" = Enclave1
type instance Endpoint "enclave2" = Enclave2

-- * inEnclave functionality

data Secure a = Secure Int (String, Int) [ByteString]

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

-- (<@>) :: (Binary a) => Secure (a -> b) -> a -> Secure b
-- (Secure id loc args) <@> arg = Secure id loc (encode arg : args)

-- * gateway functionality

-- {-# NOINLINE gateway #-}

-- {-# RULES
-- "gatewayClient1" forall (c :: Secure (Enclave1 Bool)) . gateway @Client1 c = gateway1 c
-- "gatewayClient2" forall c . gateway @Client2 c = gateway'
--   #-}

class GateWay m2 where
  gateway :: Binary a => Secure (m a) -> m2 a

instance GateWay Client2 where
  gateway _ = gateway'

-- gateway :: forall (l :: * -> *) a m m2. (Binary a) => Secure (m a) -> m2 a
-- gateway = error "gateway can not resolve destination, please indicate destination with Client1 or Client2"

gateway1 :: forall a m. (Binary a) => Secure (m a) -> Client1 a
gateway1 (Secure id (ip, port) args) = Client1 $ tryConnect ip port (id, args)

gateway' :: forall a. Client2 a
gateway' = Client2

-- * Runclient

-- runClient :: IO a -> IO ()
-- runClient ma = do
--   v <- liftIO ma
--   return $ v `seq` ()

-- runClients :: [ClientEndpoint] -> App ()
-- runClients [] = return ()
-- runClients (Endpoint (e :: m a) : xs) = case shouldIRun (Proxy @m) of
--   True -> liftIO $ runClient (toIO e)
--   False -> runClients xs

runApp :: App a -> IO a
runApp (App s) = evalStateT s (AppState 0 Map.empty)

-- * Reference management

class InEnclave f where
  inEnclave' :: (Securable a, f ~ Remote a) => a -> App (Secure a)

instance InEnclave Enclave1 where
  inEnclave' _ = inEnclave1

instance InEnclave Enclave2 where
  inEnclave' _ = inEnclave2