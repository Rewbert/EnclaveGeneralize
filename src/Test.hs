{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test where

import App
import Network
import GenModule

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import Control.Monad.State as ST
import Data.Binary
import Data.Proxy

$( configure )