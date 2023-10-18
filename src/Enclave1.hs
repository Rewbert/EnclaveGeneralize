{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Enclave1 where

import App
import Control.Monad.State as ST
import Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Data.Maybe
import Data.Proxy
import Network
import Network.Simple.TCP
import System.IO

-- * Client1

data Client1 a = Client1

instance Functor Client1 where
  fmap _ _ = Client1

instance Applicative Client1 where
  pure _ = Client1
  _ <*> _ = Client1

instance Monad Client1 where
  _ >>= _ = Client1

instance MonadIO Client1 where
  liftIO _ = Client1

instance ShouldRun Client1 where
  shouldIRun _ = False
  toIO _ = return ()

type instance Endpoint "client1" = Client1

instance GateWay Client1 where
  gateway _ = Client1





-- * Client2

data Client2 a = Client2

instance Functor Client2 where
  fmap _ _ = Client2

instance Applicative Client2 where
  pure _ = Client2
  _ <*> _ = Client2

instance Monad Client2 where
  _ >>= _ = Client2

instance MonadIO Client2 where
  liftIO _ = Client2

instance ShouldRun Client2 where
  shouldIRun _ = False
  toIO _ = return ()

type instance Endpoint "client2" = Client2

instance GateWay Client2 where
  gateway _ = Client2





-- * Enclave 1

newtype Enclave1 a = Enclave1 (IO a)
  deriving (Functor) via IO
  deriving (Applicative) via IO
  deriving (Monad) via IO
  deriving (MonadIO) via IO

type instance Endpoint "enclave1" = Enclave1

class Securable a where
  mkSecure :: a -> ([B.ByteString] -> Enclave1 B.ByteString)

instance (Binary a) => Securable (Enclave1 a) where
  mkSecure m = \_ -> fmap encode m

instance (Binary a, Securable b) => Securable (a -> b) where
  mkSecure f = \(x : xs) -> mkSecure (f $ decode x) xs

-- {-# RULES
-- "inEnclave1" forall (f :: a). inEnclave @Enclave1 f = inEnclave1 @a f
-- "inEnclaveEndpoint1" forall (f :: a). inEnclave @(Endpoint "enclave1") f = inEnclave1 @a f #-}

inEnclave1 :: forall a. (Securable a) => a -> App (Secure a)
inEnclave1 f = do
  st <- ST.get
  ST.put $ st {counter = counter st + 1, funs = Map.insert (counter st) (\bs -> let Enclave1 ma = mkSecure f bs in ma) (funs st)}
  return $ SecureDummy





-- * Enclave 2

data Enclave2 a = Enclave2

instance Functor Enclave2 where
  fmap _ _ = Enclave2

instance Applicative Enclave2 where
  pure _ = Enclave2
  _ <*> _ = Enclave2

instance Monad Enclave2 where
  _ >>= _ = Enclave2

type instance Endpoint "enclave2" = Enclave2

-- {-# RULES
-- "inEnclave2" forall (f :: a). inEnclave @Enclave2 f = inEnclave2 @a
-- "inEnclaveEndpoint2" forall (f :: a). inEnclave @(Endpoint "enclave2") f = inEnclave2 @a #-}

inEnclave2 :: forall a. (Securable a) => App (Secure a)
inEnclave2 = do
  st <- ST.get
  ST.put $ st {counter = counter st + 1}
  return $ SecureDummy





-- * inEnclave functionality

class LocationOf f => InEnclave f where
  inEnclave' :: (Securable a, f ~ Remote a) => a -> App (Secure a)

instance LocationOf Enclave1 where
  location _ = ("127.0.0.1","8002")

instance InEnclave Enclave1 where
  inEnclave' x = inEnclave1 x

instance LocationOf Enclave2 where
  location _ = ("127.0.0.1", "8003")

instance InEnclave Enclave2 where
  inEnclave' _ = inEnclave2

-- {-# NOINLINE inEnclave #-}

-- inEnclave :: forall (l :: * -> *) a. (Securable a) => a -> App (Secure a)
-- inEnclave _ = error "inEnclave applied to unresolved location indicator, please use one of Enclave1 or Enclave2"





-- * Secure

data Secure a = SecureDummy

(<@>) :: (Binary a) => Secure (a -> b) -> a -> Secure b
SecureDummy <@> _ = SecureDummy





-- * gateway functionality

class GateWay m2 where
  gateway :: Binary a => Secure (m a) -> m2 a





-- * Runclient

runClients :: [ClientEndpoint] -> App ()
runClients _ = return ()

runApp :: App a -> IO ()
runApp (App s) = do
  (_, AppState {funs = vTable}) <- runStateT s (AppState 0 Map.empty)
  serveForever ("127.0.0.1", "8002") $ Map.toList vTable