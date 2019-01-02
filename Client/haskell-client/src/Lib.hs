module Lib
  ( general
  ) where

-- base
-- import Control.Exception (bracket)
import Control.Monad (when)

import Foreign -- includes many sub-modules

-- GLFW-b, qualified for clarity
import qualified Graphics.UI.GLFW as GLFW

-- gl, all types and funcs here will already start with "gl"
import Graphics.GL.Core33
import Graphics.GL.Types

import Event
import ShaderProgram
import View
import Window (withWindow)

-- find app/ -type f | entr -r -c stack build --fast --pedantic --exec haskell-client-exe
general :: IO ()
general = do
  let width = 640
      height = 480
      hints =
        [ GLFW.WindowHint'Focused True
        , GLFW.WindowHint'Resizable False
        -- , GLFW.WindowHint'Decorated False
        , GLFW.WindowHint'ContextVersionMajor 3
        , GLFW.WindowHint'ContextVersionMinor 3
        , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
        , GLFW.WindowHint'OpenGLForwardCompat True
        ]
  GLFW.windowHint (GLFW.WindowHint'Decorated False)
  withWindow width height "GLFW-b-demo" hints $ \window -> do
    GLFW.setKeyCallback window (Just Event.listener)
            -- calibrate the viewport
    GLFW.makeContextCurrent (Just window)
    (x, y) <- GLFW.getFramebufferSize window
    glViewport 0 0 (fromIntegral x) (fromIntegral y)
    -- ready our program
    --------------------------------------------------------------------------------------------------
    eErrP <-
      ShaderProgram.programFromSources
        View.vertexShaderSource
        View.fragmentShaderSource
    shaderProgram <-
      case eErrP of
        Left e -> putStrLn e >> return 0
        Right p -> return p
    -- activate the program
    glUseProgram shaderProgram
    -- setup our verticies
    let verticies =
          [ 0.5
          , 0.5
          , 0.0 -- Top Right
          , 0.5
          , -0.5
          , 0.0 -- Bottom Right
          , -0.5
          , -0.5
          , 0.0 -- Bottom Left
          , -0.5
          , 0.5
          , 0.0 -- Top Left
          ] :: [GLfloat]
    let verticesSize =
          fromIntegral $ sizeOf (0.0 :: GLfloat) * (length verticies)
    verticesP <- newArray verticies
    -- setup the indexes
    let indices -- Note that we start from 0!
         =
          [ 0
          , 1
          , 3 -- First Triangle
          , 1
          , 2
          , 3 -- Second Triangle
          ] :: [GLuint]
    let indicesSize = fromIntegral $ sizeOf (0 :: GLuint) * (length indices)
    indicesP <- newArray indices
    -- setup a vertex array object
    vaoP <- malloc
    glGenVertexArrays 1 vaoP
    vao <- peek vaoP
    glBindVertexArray vao
    -- setup a vertex buffer object and send it data
    vboP <- malloc
    glGenBuffers 1 vboP
    vbo <- peek vboP
    glBindBuffer GL_ARRAY_BUFFER vbo
    glBufferData GL_ARRAY_BUFFER verticesSize (castPtr verticesP) GL_STATIC_DRAW
    -- setup an element buffer object and send it data
    eboP <- malloc
    glGenBuffers 1 eboP
    ebo <- peek eboP
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
    glBufferData
      GL_ELEMENT_ARRAY_BUFFER
      indicesSize
      (castPtr indicesP)
      GL_STATIC_DRAW
    -- assign the attribute pointer information
    let threeFloats = fromIntegral $ sizeOf (0.0 :: GLfloat) * 3
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE threeFloats nullPtr
    glEnableVertexAttribArray 0
    -- unbind our vertex array object to prevent accidental changes in
    -- between our draw calls.
    glBindVertexArray 0
    -- Uncomment this line for "wireframe mode"
    -- glPolygonMode GL_FRONT_AND_BACK GL_LINE
    --------------------------------------------------------------------------------------------------
    -- enter our main loop
    macFix window vao

macFix :: GLFW.Window -> GLuint -> IO ()
macFix window vao = do
  (x1, y1) <- GLFW.getWindowPos window
  GLFW.pollEvents
  GLFW.swapBuffers window
  GLFW.setWindowPos window x1 (y1 + 1)
  GLFW.setWindowPos window x1 (y1)
  loop window vao

loop :: GLFW.Window -> GLuint -> IO ()
loop window vao = do
  shouldContinue <- not <$> GLFW.windowShouldClose window
  when shouldContinue $ do
    GLFW.pollEvents -- event poll
      -- drawing
    glClearColor 0.2 0.3 0.3 1.0
    glClear GL_COLOR_BUFFER_BIT
      -- draw the triangle
    glBindVertexArray vao
    glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
    glBindVertexArray 0
      -- swap buffers and go again
    GLFW.swapBuffers window
    loop window vao
