{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
--
-- Main {{
import XMonad
-- }}

------------
-- Layouts {{
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.ThreeColumns (ThreeCol(..))
-- }}

import XMonad.Hooks.ManageHelpers (doRectFloat, isFullscreen, doFullFloat)

-- Multi toggle
import XMonad.Layout.MultiToggle (Transformer, transform, Toggle(..), mkToggle1)

-- Scratchpad
import XMonad.Util.NamedScratchpad ( NamedScratchpad(..), namedScratchpadAction
                                   , namedScratchpadManageHook, defaultFloating)

-- True fullsceen
import XMonad.Layout.NoBorders (smartBorders, noBorders)

-- Correct FS
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook, ewmh)

-- Mouse resize
import XMonad.Actions.MouseResize (mouseResize)
import XMonad.Layout.WindowArranger (windowArrange)
import XMonad.Actions.FlexibleManipulate (mouseWindow, linear)

-- StackSet
import qualified XMonad.StackSet as W

-- Mouse
--
import XMonad.Actions.UpdateFocus (focusOnMouseMove, adjustEventInput)


-- Best keybinding
import XMonad.Util.EZConfig (mkKeymap)

-- Spacing
import XMonad.Layout.Spacing (spacingRaw, Border(..))

-- Gaps
import XMonad.Layout.Gaps()

-----------
-- Windows
import XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies)
import XMonad.Actions.Navigation2D as Nav -- Window navigation

-- Move to master
import XMonad.Actions.Promote (promote)

import XMonad.Actions.CycleWS (nextWS, prevWS)

import Data.Default()
import Data.Map.Strict (Map)

import XMonad.Hooks.SetWMName (setWMName)

import XMonad.Util.Run (safeSpawn)


main :: IO()
main = do
  xmonad 
  $ Nav.withNavigation2DConfig navConf 
  $ ewmh
  $ def
    { terminal           = myTerminal
    , workspaces         = myWorkspaces
    , focusFollowsMouse  = True
    , modMask            = mod1Mask
    , keys               = myKeys
    , layoutHook         = mouseResize $ windowArrange $ myLayout
    , manageHook         = myManageHook
    , handleEventHook    = handleEventHook def  <+> focusOnMouseMove <+> fullscreenEventHook
    , startupHook        = myStartupHook
    }
  where
  navConf = def 
    { Nav.defaultTiledNavigation = centerNavigation
    }

--------------
-- Bindings --
--------------

myTerminal :: String
myTerminal = "kitty" -- Terminal

browser :: String
browser = "firefox" -- Browser

fileManager :: String
fileManager = terminalExecCommand myTerminal "env ranger" -- File manager

lockScreen :: String
lockScreen = "betterlockscreen -l" -- lockScreen

appMenu :: String
appMenu = "rofi -theme base16-nord -combi-modi drun,run -show combi -modi combi -show-icons" -- application menu

notifyCMD :: String
notifyCMD = "dunstify"

-- Screenshots
screenshotPath :: String
screenshotPath = "~/Pictures/Screenshots"

screenshot :: String
screenshot = "scrot" ++ screenshotPath

-----------
-- Utils --
-----------

-- Start command in treminal
terminalExecCommand :: String -> String -> String
terminalExecCommand t c | t == "kitty" = "kitty" ++ " " ++ c
                        | otherwise = t ++ " -e " ++ c


sendNotify :: MonadIO m => String -> m ()
sendNotify text = spawn $ notifyCMD ++ " \"" ++ text ++ "\""

statusReport :: String
statusReport = foldl (++) "" $ map (++ "\n") [ "Time:"
                                             , "$(date +%x)"
                                             , "$(date +%R)"
                                             , "Battery:"
                                             , "$(cat " ++ batPath ++ "/capacity)% ($(cat " ++ batPath ++ "/status))"
                                             ]
    where
        batPath = "/sys/class/power_supply/BAT0"

sendStatusReport :: MonadIO m => m ()
sendStatusReport = sendNotify statusReport

------------------
-- Key bindings -- 
------------------

myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys l = mkKeymap l $ concatMap ($ l) keymaps
  where
  keymaps = [ baseKeys
            , mediaKeys
            , navKeys
            , actionKeys
            , appKeys]

-- Base keys
baseKeys :: XConfig Layout -> [(String, X ())]
baseKeys _ = 
  [ ("M-S-r", spawn "xmonad --restart")
  , ("M-S-q", kill1)
  , ("M-<Space>", sendMessage NextLayout)]

-- Media keys
mediaKeys :: XConfig Layout -> [(String, X ())]
mediaKeys _ = 
  -- volume
  [ ("<XF86AudioRaiseVolume>", spawn "amixer -D pulse set Master 5%+ umute")
  , ("<XF86AudioLowerVolume>", spawn "amixer -D pulse set Master 5%- umute") 
  , ("<XF86AudioMute>", spawn "amixer -D pulse set Master toggle umute") ]

-- Navigation keys
navKeys :: XConfig Layout -> [(String, X ())]
navKeys conf =
 [ ("M-o", windows copyToAll) -- @@ Make focused window always visible
 , ("M-S-o",  killAllOtherCopies)] -- @@ Toggle window state back
 ++
 -- Window focus and swap
 [ ("M-"++modif++key, fun direct inf) 
   | (key, direct) <- directKeys
   , (fun, modif, inf) <- [ (Nav.windowSwap, "S-", False)
                          , (Nav.windowGo, "", True)]
 ]
 -- Move window to Master
 ++
 [ ("M-<L>", sendMessage Expand) 
 , ("M-<R>", sendMessage Shrink) ]
 ++
 [ ("M-]", nextWS) 
 , ("M-[", prevWS) ]
 ++
 [ ("M-S-m", promote) ]
 -- Workspace focus and move windows to workspace
 ++
 [ ("M-"++modif++wid,  windows $ fun wname) 
   | (wname, wid) <- zip (workspaces conf)
                         (map show ([1 .. 9] ++ [0]))
   , (fun, modif) <- [ (W.shift, "S-")
                     , (W.greedyView, "")]]
 where 
 directKeys = [("j", Nav.D), 
               ("k", Nav.U),
               ("l", Nav.R),
               ("h", Nav.L)]

-- Actions
actionKeys :: XConfig Layout -> [(String, X ())]
actionKeys conf =
  [ ("<Print>", spawn screenshot)
  , ("M-t", sendStatusReport) -- status
  , ("M-S-t", namedScratchpadAction scratchpads "tray") -- Tray
  , ("M-f", sendMessage $ Toggle SPFULL)
  , ("M-S-f", withFocused $ windows . W.sink)
  , ("M-r", withFocused $ mouseWindow linear)
  ]


-- Applications keys
appKeys :: XConfig Layout -> [(String, X ())]
appKeys l =
  [ ("M-b", spawn browser)
  , ("M-p", spawn lockScreen)
  , ("M-m", spawn "emacsclient -c -a '' --eval \"(mu4e)\"")
  , ("M-n", spawn "emacsclient -c -a '' --eval \"(dired \\\"~\\\")\"")
  -- , ("M-n", spawn fileManager)
  , ("M-<Return>", spawn $ terminal l)
  , ("M-d", spawn appMenu)
  ]

------------
-- Layout -- 
------------
myLayout = (id
  . smartBorders
  . mkToggle1 SPFULL
  . spacingRaw False
               (Border 6 6 6 6)
               False
               (Border 6 6 6 6)
               True
  $
  ThreeColMid 1 (3/100) (1/2) |||
  Tall 1 (10/100) (2/3) |||
  Grid |||
  Mirror (Tall 1 (3/100) (1/2))) |||
  noBorders Full

data SPFULL = SPFULL deriving (Read, Show, Eq, Typeable)
instance Transformer SPFULL Window where
    transform _ x k = k (noBorders Full) (const x)

----------------
-- Workspaces -- 
----------------
myWorkspaces :: [String]
myWorkspaces = [ "1:term"
               , "2:web"
               , "3:code"
               , "4:file"]
               ++ (map show $ [5..9] ++ [0])

-----------------
-- Manage Hook -- 
-----------------
myManageHook = composeAll
    [ className =? "stalonetray" --> doF W.focusDown
    , resource =? "Toolkit" <&&> (className =? "firefox" <||> className =? "Firefox") --> doFloat
    , resource =? "Navigator" <&&> (className =? "firefox" <||> className =? "Firefox") --> doFloat
    , className =? "MEGAsync" --> doFloat
    , className =? "mpv" --> doRectFloat (W.RationalRect 0.25 0.25 0.5 0.5)
    -- , ((className =? "Navigator") <&&> (className =? "firefox")) --> doFloat
    -- , title =? "Extension: (Tree Style Tab) - Close tabs? - Mozilla Firefox" --> doFloat
    , isFullscreen --> doFullFloat
    , namedScratchpadManageHook scratchpads 
    ]

------------------
-- Startup Hook --
------------------
myStartupHook :: X ()
myStartupHook = spawnHooks <+> adjustEventInput <+> setWMName "LG3D"
  where
    spawnHooks = foldl1 (>>) $ map (\x -> spawn x) commands
    commands = [ "compton"
               , "nitrogen --restore"
               , "blueman-applet"
               , "keynav"]
           
---------------
-- Sratchpad --
---------------
scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "tray" "stalonetray" (className =? "stalonetray") defaultFloating
  ]
