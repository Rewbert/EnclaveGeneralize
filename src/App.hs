{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeOperators #-}
{- | App code that is static -}
module App where

import Control.Monad.State
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Data.IORef
import Data.Proxy
import Network -- for the CallID and Function types

data AppState = AppState {counter :: Int, funs :: Map.Map Int ([B.ByteString] -> IO B.ByteString)} -- FIXME

initAppState :: AppState
initAppState = AppState 0 Map.empty

runAppClient :: App a -> IO ()
runAppClient (App s) = evalStateT s initAppState >> return ()

runAppServer :: App a -> IO [(CallID, Function)]
runAppServer (App s) = do
  (_, AppState {funs = vTable}) <- runStateT s initAppState
  return $ Map.toList vTable

newtype App a = App (StateT AppState IO a)
  deriving (Functor) via (StateT AppState IO)
  deriving (Applicative) via (StateT AppState IO)
  deriving (Monad) via (StateT AppState IO)
  deriving (MonadState AppState) via (StateT AppState IO)
  deriving (MonadIO) via (StateT AppState IO)

class ShouldRun (m :: * -> *) where
  shouldIRun :: proxy m -> Bool
  toIO :: m a -> IO ()

class LocationOf (m :: * -> *) where
    location :: proxy m -> (String, String)

data ClientEndpoint where
  Endpoint :: (ShouldRun m) => m a -> ClientEndpoint

type family Endpoint (l :: k) :: * -> *

type family Remote a where
    Remote (a -> b) = Remote b
    Remote (m a) = m

newtype Ref a = Ref (IORef a)

class ServerRef f where
  newRef :: a -> App (f (Ref a))
  setRef :: Ref a -> a -> f ()
  getRef :: Ref a -> f a