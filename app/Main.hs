module Main where

import HasTEE

import Control.Monad.IO.Class

testEnclave1 :: String -> Enclave1 String
testEnclave1 str = return $ str <> " world"

client1 :: Secure (String -> Enclave1 String) -> Client1 ()
client1 api = do
  resp <- gateway $ api <@> "hello"
  liftIO $ putStrLn resp

app :: App ()
app = do
  f <- inEnclave testEnclave1
  runClients [Endpoint (client1 f)]

main :: IO ()
main = runApp app