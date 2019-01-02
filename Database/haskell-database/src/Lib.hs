{-# LANGUAGE RecordWildCards #-}

module Lib
  ( someFunc
  ) where
    -- https://stackoverflow.com/questions/11905826/haskell-data-binary-example

import qualified Data.ByteString.Lazy as B

-- import qualified Data.ByteString as B
-- import Data.Flat
-- import qualified  Data.Iteratee as I
-- import qualified Data.Attoparsec.Iteratee as I
-- import qualified Data.Attoparsec.Char8 as P
-- import Control.Monad.IO.Class
-- import System.Environment
-- main :: IO ()
-- someFunc = do
--   [file] <- getArgs
--   flip I.fileDriverRandom file $ do
--     I.seek 20
--     num1 <- I.parserToIteratee P.number
--     liftIO $ print num1
--     I.seek 10
--     num2 <- I.parserToIteratee P.number
--     liftIO $ print num2
import Data.Binary

-- import Block
-- import Data.ByteString
import System.IO

type Name = String

type Address = String

type Phone = String

data Contacts =
  Contacts [(Name, Address)]
  deriving (Show)

instance Binary Contacts where
  put (Contacts set) = put set
  get = fmap Contacts get

data Contact = Contact
  { name :: Name
  , address :: Address
  , phone :: Phone
  } deriving (Show)

instance Binary Contact where
  put Contact {..} = do
    put name
    put address
    put phone
  get = do
    name <- get
    address <- get
    phone <- get
    return Contact {..}

-- getFileContents :: Handle -> IO ()
-- getFileContents fileHandle = do
--   isEofFile <- hIsEOF fileHandle
--   if isEofFile
--     then return ()
--     else do
--       info <- hGetLine fileHandle
--       putStrLn info
--       getFileContents fileHandle
writeData :: FilePath -> B.ByteString -> IO ()
writeData file content = do
  handle <- openBinaryFile file WriteMode
  B.hPut handle content
  hClose handle

readData :: FilePath -> IO (B.ByteString)
readData file = do
  handle <- openBinaryFile file ReadMode
  a <- B.hGet handle 100
  hClose handle
  return a

someFunc :: IO ()
someFunc
  -- File work https://books.google.lv/books?id=nh0okI1a1sQC&pg=PA172&lpg=PA172&dq=haskell+binary+file+seek&source=bl&ots=s_VEwclRbM&sig=DgIU-1yQlrJX2ZIKPyGh1G_Dkx0&hl=en&sa=X&ved=2ahUKEwiLj7H6iK7eAhXBWywKHStABFIQ6AEwCHoECAIQAQ#v=onepage&q=haskell%20binary%20file%20seek&f=false
  -- handle <- openBinaryFile "girlfriend.txt" ReadWriteMode
  -- contents <- hGetContents handle
  -- delme <- hTell handle
  -- putStrLn contents
  -- -- putStrLn $ show "\ndelme:"
  -- putStrLn $ "delme:" ++ show delme
  -- -- print delme
  -- hClose handle
  -- putStrLn "Enter file name (Including full path) to read"
  -- fileName <- getLine
 = do
  let c = Contacts [("gert", "home"), ("gert2", "home2")]
  let e = encode c
  print e
  -- let d = decode e
  -- print (d :: Contacts)
  -- let c' = Contact {name = "gert", address = "home", phone = "test"}
  -- let e' = encode c'
  -- print e'
  -- let d' = decode e'
  -- print (d' :: Contact)
  -- let delme =
  --       Header
  --         {length = 000000112, kind = 10, pointerToData = 0, pointerToNext = 0}
  --     testData = flat delme
  writeData "Bar2.dat" e
  a <- readData "Bar2.dat"
  -- handle <- openBinaryFile "Bar.dat" ReadWriteMode
  -- B.hPut handle e
  -- a <- B.hGet handle $ 100
  -- let delme3 = unflat a :: Decoded Header
  -- print $ B.length testData
  -- print testData
  let d = decode a :: Contacts
  print "AAAA"
  print $ d
  -- print $ delme3
  -- hClose handle
