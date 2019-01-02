module Demo.Info
  ( printInstructions
  , printEvent
  , showModifierKeys
  , printInformation
  ) where

import Control.Monad.RWS.Strict (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Demo.Alias (Demo)
import qualified Graphics.UI.GLFW as GLFW
import Text.PrettyPrint hiding ((<>))

printInstructions :: IO ()
printInstructions =
  putStrLn $
  render $
  nest
    4
    (text "------------------------------------------------------------" $+$
     text "'?': Print these instructions" $+$
     text "'i': Print GLFW information" $+$
     text "" $+$
     text "* Mouse cursor, keyboard cursor keys, and/or joystick" $+$
     text "  control rotation." $+$
     text "* Mouse scroll wheel controls distance from scene." $+$
     text "------------------------------------------------------------")

printInformation :: GLFW.Window -> IO ()
printInformation win = do
  version <- GLFW.getVersion
  versionString <- GLFW.getVersionString
  monitorInfos <- runMaybeT getMonitorInfos
  joystickNames <- getJoystickNames
  clientAPI <- GLFW.getWindowClientAPI win
  cv0 <- GLFW.getWindowContextVersionMajor win
  cv1 <- GLFW.getWindowContextVersionMinor win
  cv2 <- GLFW.getWindowContextVersionRevision win
  robustness <- GLFW.getWindowContextRobustness win
  forwardCompat <- GLFW.getWindowOpenGLForwardCompat win
  debug <- GLFW.getWindowOpenGLDebugContext win
  profile <- GLFW.getWindowOpenGLProfile win
  putStrLn $
    render $
    nest
      4
      (text "------------------------------------------------------------" $+$
       text "GLFW C library:" $+$
       nest
         4
         (text "Version:" <+>
          renderVersion version $+$ text "Version string:" <+>
          renderVersionString versionString) $+$
       text "Monitors:" $+$
       nest 4 (renderMonitorInfos monitorInfos) $+$
       text "Joysticks:" $+$
       nest 4 (renderJoystickNames joystickNames) $+$
       text "OpenGL context:" $+$
       nest
         4
         (text "Client API:" <+>
          renderClientAPI clientAPI $+$ text "Version:" <+>
          renderContextVersion cv0 cv1 cv2 $+$ text "Robustness:" <+>
          renderContextRobustness robustness $+$ text "Forward compatibility:" <+>
          renderForwardCompat forwardCompat $+$ text "Debug:" <+>
          renderDebug debug $+$ text "Profile:" <+> renderProfile profile) $+$
       text "------------------------------------------------------------")
  where
    renderVersion (GLFW.Version v0 v1 v2) =
      text $ intercalate "." $ map show [v0, v1, v2]
    renderVersionString = text . show
    renderMonitorInfos = maybe (text "(error)") (vcat . map renderMonitorInfo)
    renderMonitorInfo (name, (x, y), (w, h), vms) =
      text (show name) $+$
      nest 4 (location <+> size $+$ fsep (map renderVideoMode vms))
      where
        location = int x <> text "," <> int y
        size = int w <> text "x" <> int h <> text "mm"
    renderVideoMode (GLFW.VideoMode w h r g b rr) =
      brackets $ res <+> rgb <+> hz
      where
        res = int w <> text "x" <> int h
        rgb = int r <> text "x" <> int g <> text "x" <> int b
        hz = int rr <> text "Hz"
    renderJoystickNames pairs =
      vcat $ map (\(js, name) -> text (show js) <+> text (show name)) pairs
    renderContextVersion v0 v1 v2 =
      hcat [int v0, text ".", int v1, text ".", int v2]
    renderClientAPI = text . show
    renderContextRobustness = text . show
    renderForwardCompat = text . show
    renderDebug = text . show
    renderProfile = text . show

type MonitorInfo = (String, (Int, Int), (Int, Int), [GLFW.VideoMode])

getMonitorInfos :: MaybeT IO [MonitorInfo]
getMonitorInfos = getMonitors >>= mapM getMonitorInfo
  where
    getMonitors :: MaybeT IO [GLFW.Monitor]
    getMonitors = MaybeT GLFW.getMonitors
    getMonitorInfo :: GLFW.Monitor -> MaybeT IO MonitorInfo
    getMonitorInfo mon = do
      name <- getMonitorName mon
      vms <- getVideoModes mon
      MaybeT $ do
        pos <- liftIO $ GLFW.getMonitorPos mon
        size <- liftIO $ GLFW.getMonitorPhysicalSize mon
        return $ Just (name, pos, size, vms)
    getMonitorName :: GLFW.Monitor -> MaybeT IO String
    getMonitorName mon = MaybeT $ GLFW.getMonitorName mon
    getVideoModes :: GLFW.Monitor -> MaybeT IO [GLFW.VideoMode]
    getVideoModes mon = MaybeT $ GLFW.getVideoModes mon

getJoystickNames :: IO [(GLFW.Joystick, String)]
getJoystickNames = catMaybes `fmap` mapM getJoystick joysticks
  where
    getJoystick js =
      fmap (maybe Nothing (\name -> Just (js, name))) (GLFW.getJoystickName js)
    --------------------------------------------------------------------------------

printEvent :: String -> [String] -> Demo ()
printEvent cbname fields = liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk = "[mod keys: " ++ keys ++ "]"
  where
    keys =
      if null xs
        then "none"
        else unwords xs
    xs = catMaybes ys
    ys =
      [ if GLFW.modifierKeysShift mk
          then Just "shift"
          else Nothing
      , if GLFW.modifierKeysControl mk
          then Just "control"
          else Nothing
      , if GLFW.modifierKeysAlt mk
          then Just "alt"
          else Nothing
      , if GLFW.modifierKeysSuper mk
          then Just "super"
          else Nothing
      ]
    --------------------------------------------------------------------------------

joysticks :: [GLFW.Joystick]
joysticks =
  [ GLFW.Joystick'1
  , GLFW.Joystick'2
  , GLFW.Joystick'3
  , GLFW.Joystick'4
  , GLFW.Joystick'5
  , GLFW.Joystick'6
  , GLFW.Joystick'7
  , GLFW.Joystick'8
  , GLFW.Joystick'9
  , GLFW.Joystick'10
  , GLFW.Joystick'11
  , GLFW.Joystick'12
  , GLFW.Joystick'13
  , GLFW.Joystick'14
  , GLFW.Joystick'15
  , GLFW.Joystick'16
  ]
