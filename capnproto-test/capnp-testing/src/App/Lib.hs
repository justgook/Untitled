{-# LANGUAGE OverloadedStrings #-}

module App.Lib
  ( someFunc
  ) where

import Capnp.Addressbook.Pure

-- import qualified Data.ByteString.Lazy as B
import Data.Capnp (def, defaultLimit, hGetValue, hPutValue, putValue)
import qualified Data.Vector as V
import System.IO

writeData :: FilePath -> AddressBook -> IO ()
writeData file content = do
  handle <- openBinaryFile file WriteMode
  hPutValue handle content
  hClose handle

readData :: FilePath -> IO (AddressBook)
readData file = do
  handle <- openBinaryFile file ReadMode
  a <- hGetValue handle defaultLimit
  hClose handle
  return a

someFunc :: IO ()
someFunc = do
  let testData =
        AddressBook
          { people =
              V.fromList
                [ Person
                    { Capnp.Addressbook.Pure.id = 123
                    , name = "Alice"
                    , email = "alice@example.com"
                    , phones =
                        V.fromList
                          [ def
                              { number = "555-1212"
                              , type_ = Person'PhoneNumber'Type'mobile
                              }
                          ]
                    , employment = Person'employment'school "MIT"
                    }
                , Person
                    { Capnp.Addressbook.Pure.id = 456
                    , name = "Bob"
                    , email = "bob@example.com"
                    , phones =
                        V.fromList
                          [ def
                              { number = "555-4567"
                              , type_ = Person'PhoneNumber'Type'home
                              }
                          , def
                              { number = "555-7654"
                              , type_ = Person'PhoneNumber'Type'work
                              }
                          ]
                    , employment = Person'employment'selfEmployed
                    }
                ]
          }
  writeData "hello.dat" testData
  a <- readData "hello.dat"
  putValue a
  print a
