{-# LANGUAGE OverloadedStrings #-}

-- Hello World server
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.ZMQ4.Monadic
  ( Rep(Rep)
  , bind
  , liftIO
  , receive
  , runZMQ
  , send
  , socket
  )

main :: IO ()
main =
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
