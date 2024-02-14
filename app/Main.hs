-- {-# OPTIONS_GHC -fplugin Plugin #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           HasTEE


import           Data.IORef

import           Control.Monad.Trans
import           Data.Text.Lazy      (pack)
import           GHC.IO              (unsafePerformIO)


import           Unsafe.Coerce       (unsafeCoerce)
import           Web.Scotty

testEnclave1 :: Enclave1 (Ref Int) -> String -> Enclave1 String
testEnclave1 r str = do
  ref <- r
  v <- getRef ref
  setRef ref $ v + 1
  return (str <> " world " <> show v)

client1 :: Secure (String -> Enclave1 String) -> Client1 ()
client1 api = do
  liftIO $ scotty 3000 $ do
    get "/:word" $ do
      text <- liftClient1 (gateway $ api <@> "hello")
      html $ mconcat ["<h1>Server response: ", pack text, "</h1>"]

app :: App ()
app = do
  enclave1st <- newRef (0 :: Int)
  f <- inEnclave (testEnclave1 enclave1st)
  runClients [Endpoint (client1 f)]

main :: IO ()
main = runApp app
