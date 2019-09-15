{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
--
-- Main {{
import XMonad
-- }}
import XMonad.Actions.DeManage
------------
-- Layouts {{
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.ThreeColumns (ThreeCol(..))
-- }}

import XMonad.Hooks.ManageHelpers

import XMonad.Hooks.FloatNext

import XMonad.Layout.ResizableTile

-- Multi toggle
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

-- Scratchpad
import XMonad.Util.NamedScratchpad

-- True fullsceen
import XMonad.Layout.NoBorders

-- Correct FS
import XMonad.Hooks.EwmhDesktops

-- StackSet
import qualified XMonad.StackSet as W

-- Mouse
import XMonad.Actions.UpdateFocus


-- Best keybinding
import XMonad.Util.EZConfig (mkKeymap)

-- Spacing
import XMonad.Layout.Spacing (toggleWindowSpacingEnabled, toggleScreenSpacingEnabled, spacingRaw, Border(..))

-- Gaps
import XMonad.Layout.Gaps (gaps)

-----------
-- Windows
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.Navigation2D as Nav-- Window navigation

-- Move to master
import XMonad.Actions.Promote (promote)


import Data.Default (def)
import Data.Map.Strict (Map)

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
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , handleEventHook    = handleEventHook def <+> fullscreenEventHook <+> focusOnMouseMove
    , startupHook        = myStartupHook
    }
  where
  navConf = def 
    { Nav.defaultTiledNavigation = centerNavigation
    }

--------------
-- Bindings --
--------------

myTerminal = "kitty" -- Terminal
browser = "firefox" -- Browser
fileManager = terminalExecCommand myTerminal "ranger" -- File manager
lockScreen = "betterlockscreen -l" -- lockScreen
appMenu = "rofi -combi-modi drun,run -show combi -modi combi -show-icons" -- application menu
notifyCMD = "dunstify"

-- Screenshots
screenshotPath = "~/Pictures/Screenshots"
screenshot = "scrot" ++ screenshotPath 

-----------
-- Utils --
-----------

-- Start command in treminal
terminalExecCommand :: String -> String -> String
terminalExecCommand t c | t == "kitty" = "kitty" ++ " " ++ c
                        | otherwise = t ++ " -e " ++ c

 
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
  , ("M-t", spawn (notifyCMD ++ " \"Time:\" \"$(date +%x)\n$(date +%R)\"")) -- time
  , ("M-S-t", namedScratchpadAction scratchpads "tray") -- Tray
  , ("M-f", sendMessage $ Toggle FULL)
  , ("M-S-f", withFocused $ windows . W.sink)
  ]


-- Applications keys
appKeys :: XConfig Layout -> [(String, X ())]
appKeys l =
  [ ("M-b", spawn browser) , ("M-p", spawn lockScreen)
  , ("M-m", spawn fileManager)
  , ("M-<Return>", spawn $ terminal l)
  , ("M-d", spawn appMenu)
  ]

------------
-- Layout -- 
------------
myLayout = (id
  . smartBorders
  . mkToggle1 FULL
  . spacingRaw False
               (Border 6 6 6 6)
               False
               (Border 6 6 6 6)
               True
  $ Tall 1 (10/100) (2/3) |||
    Grid |||
    ThreeColMid 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)))
  |||
  noBorders Full

----------------
-- Workspaces -- 
----------------
myWorkspaces = [ "1:term"
               , "2:web"
               , "3:code"
               , "4:file"]
               ++ (map show ([5..9] ++ [0]))

-----------------
-- Manage Hook -- 
-----------------
myManageHook = composeAll
    [ className =? "stalonetray" --> doF W.focusDown
    , isFullscreen --> doFullFloat
    , namedScratchpadManageHook scratchpads ]

------------------
-- Startup Hook --
------------------
myStartupHook = spawnHooks <+> adjustEventInput
  where
    spawnHooks = foldl1 (>>) $ map (\x -> spawn x) commands
    commands = [ "compton"
               , "nitrogen --restore"
               , "keynav"]
           
---------------
-- Sratchpad --
---------------
scratchpads =
  [ NS "tray" "stalonetray" (className =? "stalonetray") defaultFloating
  ]
