-- {-# OPTIONS_GHC -fplugin Plugin #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           HasTEE

import           Control.Monad.IO.Class

import           Data.IORef

import           Data.Text.Lazy         (pack)
import           GHC.IO                 (unsafePerformIO)
import           Web.Scotty


--import           Web.Scotty.Trans
--
--type Sclient = ScottyT Client1


testEnclave1 :: Enclave1 (Ref Int) -> String -> Enclave1 String
testEnclave1 r str = do
  ref <- r
  v <- getRef ref
  setRef ref $ v + 1
  return (str <> " world " <> show v)

dapperFella :: Client1 a -> IO a
dapperFella (Client1 a) = a

client1 :: Secure (String -> Enclave1 String) -> Client1 ()
client1 api = do
  -- liftIO $ scottyT 3000 id $ do
  --   resp <- gateway $ api <@> "hello"
  --   get "/:word" $ do
  --     html $ mconcat ["<h1>Scotty, ", resp, " me up!</h1>"]
  let god = (gateway $ api <@> "helliftlo") :: Client1 String
  let text = pack "yo"
  liftIO $ scotty 3000 $ do
    text <- liftIO $ unsafePerformIO $ dapperFella god
    let str = pack text
    get "/:word" $ do
        html $ mconcat ["<h1>Scotty, ", str, " me up!</h1>"]
       --beam <- pathParam "word"

-- test :: IO ()
-- test = scotty 3000 $
--   get "/:word" $ do
--     html $ mconcat ["<h1>Scotty, ", "yaob", " me up!</h1>"]
app :: App ()
app = do
  enclave1st <- newRef (0 :: Int)
  f <- inEnclave (testEnclave1 enclave1st)
  runClients [Endpoint (client1 f)]

main :: IO ()
main = runApp app
