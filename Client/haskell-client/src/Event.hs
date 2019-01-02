module Event
  ( listener
  ) where

import Control.Monad (when)
import qualified Graphics.UI.GLFW as GLFW

listener :: GLFW.KeyCallback
    -- callback window key scanCode keyState modKeys = do
listener window key _ keyState _ = do
  print key
  when
    (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
    (GLFW.setWindowShouldClose window True)
