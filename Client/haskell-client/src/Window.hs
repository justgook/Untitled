module Window
  ( withWindow
  ) where

import Control.Monad (when)
import Data.List as List
import qualified Graphics.UI.GLFW as GLFW

--
-- GLFW-b is made to be very close to the C API, so creating a window is pretty
-- clunky by Haskell standards. A higher-level API would have some function
-- like withWindow.
withWindow ::
     Int
  -> Int
  -> String
  -> [GLFW.WindowHint]
  -> (GLFW.Window -> IO ())
  -> IO ()
withWindow width height title hints f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    _ <- sequence $ List.map GLFW.windowHint hints
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      (Just win) -> do
        GLFW.makeContextCurrent m
        f win
        GLFW.setErrorCallback $ Just simpleErrorCallback
        GLFW.destroyWindow win
      Nothing -> return ()
    GLFW.terminate
  where
    simpleErrorCallback e s = putStrLn $ unwords [show e, show s]
