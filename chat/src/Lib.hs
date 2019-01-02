{-# LANGUAGE OverloadedStrings #-} -- maybe get rid of it?

module Lib
  ( someFunc
  ) where

import qualified Capnp.Protocol.Pure as P
import Control.Concurrent
import Control.Monad
import Data.Capnp (def, putValue)
import Data.Capnp.Basics.Pure as C
import System.ZMQ4.Monadic

-- import qualified Data.Text as T
someFunc :: IO ()
someFunc = do
  let testData =
        P.Package
          { P.from = P.Target'global
          , P.to = P.Target'global
          , P.timestamp = 123
          , P.content = P.Package'content'message "Chat Message"
          }
  print testData
