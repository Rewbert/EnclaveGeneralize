{-# LANGUAGE InstanceSigs #-}
-- {-# OPTIONS_GHC -fplugin Plugin #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Concurrent (
  forkIO,
  newChan,
  readChan,
  threadDelay,
  writeChan,
 )
import Control.Concurrent.Async
import Control.Concurrent.Chan (readChan)
import Control.Monad (foldM, forM, forM_, unless)
import Control.Monad.Trans
import Data.Binary (Binary (..), Get, Put, Word8)
import Data.Binary.Get (getByteString, getInt32host, getInt64host, getInthost, getWord32host)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSLC
import Data.IORef
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (pack, unpack)
import Debug.Trace (traceShow, traceShowId)
import HasTEE
import Test.QuickCheck

type AccountNr = Int
type Balance = Int
type Workers = [(WorkerId, Status)]
type WorkerId = Int
data Status = Working | Waiting

data WorkOrder
  = MapOrder Int64
  | ReduceOrder [String]

instance Binary WorkOrder where
  put :: WorkOrder -> Put
  put (MapOrder i) = put (0 :: Word8) <> put i
  put (ReduceOrder s) = put (1 :: Word8) <> putList s

  get :: Get WorkOrder
  get = do
    int <- get @Word8
    if int == 0
      then MapOrder . fromIntegral <$> get @Int64
      else ReduceOrder <$> get

{- | Exclaimer
Implementing map reduce with multiple workers is most likely not possible
as it requires requests to be handled asynchronously.

Scratch that, it seems that SGX allows threading in some capacity.
-}
work :: WorkOrder -> IO String
work (MapOrder i) = do
  char <- generate arbitrary :: IO Char
  pure $ traceShowId $ replicate (fromIntegral $ traceShowId i) char
work (ReduceOrder s) = undefined

processWorker1 :: WorkOrder -> Worker1 String
processWorker1 = liftIO . work

processWorker2 :: WorkOrder -> Worker2 String
processWorker2 = liftIO . work

-- since enclaves can not safetly call other encalaves in the context of IO this is a dud
overlord :: Overlord (Ref Workers) -> Secure (WorkOrder -> Worker1 String) -> Secure (WorkOrder -> Worker2 String) -> Int64 -> Overlord String
overlord w'db'ref api1 api2 amount = do
  w'db <- w'db'ref
  w'db <- getRef w'db

  let callApi1 = gateway $ api1 <@> MapOrder amount

  chan <- liftIO newChan

  forM_ [0 .. amount] $ \_ ->
    liftIO $ forkIO $ do
      res <- callApi1
      writeChan chan res

  mapResults <- forM [0 .. amount] $ \_ -> liftIO . readChan $ chan
  pure $ concat mapResults

client1 :: Secure (Int64 -> Overlord String) -> Client1 ()
client1 api = do
  liftIO $
    mapM_
      ( \i -> do
          putStrLn $ "Work iteration: " <> show i
          let workCount = 3

          chan <- newChan

          -- Alright for the client to use threads, as I imagine it
          -- runs in a less restricted environment compared to an enclave.
          res <- forM [1 .. workCount] $ \c -> do
            forkIO $ do
              req <- flip mod 16 . abs <$> generate arbitrary
              putStrLn $ "Client " <> show c <> " requesting work: " <> show req
              resp <- liftClient1 (gateway $ api <@> req)
              putStrLn $ "Client " <> show c <> " got result " <> show resp
              writeChan chan ()

          forM_ [1 .. 3] $ \_ -> readChan chan

          putStrLn ""

          threadDelay 1000000
      )
      [1 ..]
  pure ()

app :: App ()
app = do
  enclave1st <- newRef $ map (,Waiting) [0 .. 1]
  w1 <- inEnclave processWorker1
  w2 <- inEnclave processWorker2
  ol <- inEnclave (overlord enclave1st w1 w2)
  runClients [Endpoint (client1 ol)]

main :: IO ()
main = runApp app
