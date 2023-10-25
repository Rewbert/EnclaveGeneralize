{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{- | This module parses the configuration file and splices in code for the different
endpoints. The API is otherwise very similar to HasTEE as described in the seminal paper. -}
module HasTEE (module HasTEE, module App, module Network) where

import App
import Control.Monad.IO.Class
import Control.Monad.State as ST
import Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Data.Proxy
import Data.IORef
import GenModule
import Network

$(configure)