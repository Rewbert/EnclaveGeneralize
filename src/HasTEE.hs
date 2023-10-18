{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HasTEE (module HasTEE, module App, module Network, configure) where

import App
import Control.Monad.IO.Class
import Control.Monad.State as ST
import Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Data.Proxy
import GenModule
import Network

$(configure)