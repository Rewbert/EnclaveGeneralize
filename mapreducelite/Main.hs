-- {-# OPTIONS_GHC -fplugin Plugin #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad (forM, forM_, unless)
import Control.Monad.Trans
import Data.IORef
import Data.Text.Lazy (pack)
import HasTEE
import Test.QuickCheck

type AccountNr = Int
type Balance = Int
type Workers = [(WorkerId, Status)]
type WorkerId = Int
data Status = Working | Waiting

{- | Exclaimer
Implementing map reduce with multiple workers is most likely not possible
as it requires requests to be handled asynchronously
-}
processWorker1 :: Int -> Worker1 Int
processWorker1 magic = liftIO $ generate arbitrary

processWorker2 :: Int -> Worker2 Int
processWorker2 magic = liftIO $ generate arbitrary

overlord :: Overlord (Ref Workers) -> Secure (Int -> Worker1 Int) -> Secure (Int -> Worker2 Int) -> Int -> Overlord Int
overlord w'db'ref api1 api2 value = do
  w'db <- w'db'ref
  w'db <- getRef w'db

  -- If possible I want to avoid using forkIO/async, as
  -- I want this to work without requiring OS threads.
  res <- gateway $ api1 <@> value

  liftIO $ withAsync (pure ()) $ \a -> do
    res <- wait a
    liftIO $ print res

  pure res

client1 :: Secure (Int -> Overlord Int) -> Client1 ()
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
              putStrLn $ "Client " <> show c <> " requesting work"
              resp <- liftClient1 (gateway $ api <@> 0)
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
  enclave1st <- newRef [(0, Waiting), (1, Waiting)]
  w1 <- inEnclave processWorker1
  w2 <- inEnclave processWorker2
  ol <- inEnclave (overlord enclave1st w1 w2)
  runClients [Endpoint (client1 ol)]

main :: IO ()
main = runApp app
