module Main where

import HasTEE

import Control.Monad.IO.Class

testEnclave2 :: String -> Enclave2 String
testEnclave2 str = return $ str <> " from enclave 2!"

testEnclave1 :: Secure (String -> Enclave2 String) -> String -> Enclave1 String
testEnclave1 api str = gateway $ api <@> (str <> " world")

client1 :: Secure (String -> Enclave1 String) -> Client1 ()
client1 api = do
  resp <- gateway $ api <@> "hello"
  liftIO $ putStrLn resp

app :: App ()
app = do
  g <- inEnclave testEnclave2
  f <- inEnclave (testEnclave1 g)
  runClients [Endpoint (client1 f)]

main :: IO ()
main = runApp app