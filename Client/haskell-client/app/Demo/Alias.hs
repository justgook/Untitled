module Demo.Alias
  ( Demo
  , Env(..)
  , State(..)
  ) where

import Control.Concurrent.STM (TQueue)
import Control.Monad.RWS.Strict (RWST)
import Demo.Event (Event)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

type Demo = RWST Env () State IO

data Env = Env
  { envEventsChan :: TQueue Event
  , envWindow :: !GLFW.Window
  , envGear1 :: !GL.DisplayList
  , envGear2 :: !GL.DisplayList
  , envGear3 :: !GL.DisplayList
  , envZDistClosest :: !Double
  , envZDistFarthest :: !Double
  }

data State = State
  { stateWindowWidth :: !Int
  , stateWindowHeight :: !Int
  , stateXAngle :: !Double
  , stateYAngle :: !Double
  , stateZAngle :: !Double
  , stateGearZAngle :: !Double
  , stateZDist :: !Double
  , stateMouseDown :: !Bool
  , stateDragging :: !Bool
  , stateDragStartX :: !Double
  , stateDragStartY :: !Double
  , stateDragStartXAngle :: !Double
  , stateDragStartYAngle :: !Double
  }
