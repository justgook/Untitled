{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Block
  ( Block
  , Header(..)
  ) where

import Data.Flat

data Block = Comment
  { header :: Header
  , meta :: BlockMeta
  -- , data :: Data
  } deriving (Show)

data Header = Header
  { kind :: Int
  , length :: Int
  } deriving (Show, Generic, Flat)

data BlockMeta = BlockMeta
  { index :: Int
  , timeStamp :: Int
  , author :: Author
  , nonce :: Int
  , previousHash :: Hash
  , hash :: Hash
  } deriving (Show, Generic, Flat)

-- type Pointer = Int
type Hash = String

type Author = String
