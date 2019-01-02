{-# LANGUAGE OverloadedStrings #-}

-- Hello World server
module HWserver where

import Control.Concurrent
import Control.Monad
import System.ZMQ4.Monadic

hwserver :: IO ()
hwserver =
  runZMQ $
    -- Socket to talk to clients
   do
    responder <- socket Rep
    bind responder "tcp://*:5555"
    forever $ do
      buffer <- receive responder
      liftIO $ do
        putStrLn "Received Hello"
        threadDelay 1000000 -- Do some 'work'
      send responder [] "World"
