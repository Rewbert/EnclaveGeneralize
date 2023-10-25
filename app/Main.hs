{-# OPTIONS_GHC -fplugin Plugin #-}
module Main where

import HasTEE

import Control.Monad.IO.Class

import Data.IORef

testEnclave1 :: Enclave1 (Ref Int) -> String -> Enclave1 String
testEnclave1 r str = do
  ref <- r
  v <- getRef ref
  setRef ref $ v + 1
  return (str <> " world " <> show v)

client1 :: Secure (String -> Enclave1 String) -> Client1 ()
client1 api = do
  resp <- gateway $ api <@> "hello"
  liftIO $ putStrLn resp

app :: App ()
app = do
  enclave1st <- newRef (0 :: Int)
  f <- inEnclave (testEnclave1 enclave1st)
  runClients [Endpoint (client1 f)]

main :: IO ()
main = runApp app