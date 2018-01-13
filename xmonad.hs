import qualified Data.List as List (concat, intercalate, isInfixOf)
import qualified Data.Map as Map (fromList, Map)

import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioMute, xF86XK_AudioRaiseVolume, xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp)

import Network.HostName (getHostName)

import System.Exit ()
import System.IO (hPutStrLn)

import Text.Printf (printf)

import XMonad 
import XMonad.Actions.CycleWS (doTo, moveTo, nextScreen, shiftNextScreen, WSType(EmptyWS))
import XMonad.Actions.GroupNavigation (historyHook, nextMatch, nextMatchOrDo, Direction(Backward, Forward, History))
import XMonad.Actions.UpdatePointer (updatePointer)

import XMonad.Hooks.DynamicLog (dynamicLogWithPP, pad, ppCurrent, ppHidden, ppLayout, ppOutput, ppSep, ppTitle, ppWsSep, shorten, xmobarColor)
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsStartup, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, docksEventHook, manageDocks, ToggleStruts(ToggleStruts))
import XMonad.Hooks.ManageHelpers (doCenterFloat, isDialog)
import XMonad.Hooks.SetWMName (setWMName)

import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout.NoBorders (noBorders, smartBorders)

import XMonad.Prompt (deleteConsecutive, Direction1D(Next), XPPosition(..), XPConfig(..))
import XMonad.Prompt.Shell (shellPrompt)
import qualified XMonad.StackSet as StackSet (focusDown, focusMaster, focusUp, greedyView, shift, shiftMaster, sink, swapDown, swapMaster, swapUp)

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

-----------------------------------------------------------------------
-- Customizations
-----------------------------------------------------------------------

myTerminal :: String
myTerminal      = "urxvt256c"

myTerminalClass :: String
myTerminalClass = "URxvt"

myBrightnessStep :: String
myBrightnessStep        = "5"

myBrightnessDefaultLow :: String
myBrightnessDefaultLow  = "15"

myBrightnessDefaultHigh :: String
myBrightnessDefaultHigh = "50"

myNormalBorderColor :: String
myNormalBorderColor  = "#dcdccc"

myFocusedBorderColor :: String
myFocusedBorderColor = "#de7168"

myXFTFont :: String
myXFTFont       = "xft:DejaVu Sans Mono-10:antialias=true"

myXmobarFgColor :: String
myXmobarFgColor = "#c7c7c7"

myXmobarBgColor :: String
myXmobarBgColor = "#000000"

myXmobarHiColor :: String
myXmobarHiColor = "#333333"

myXmobarColorRed :: String
myXmobarColorRed = "#ff8278"

myXmobarColorGrn :: String
myXmobarColorGrn = "#d6fcba"

xmobarPipeAudio :: FilePath
xmobarPipeAudio = "/home/jirik/.xmonad/xmobar-pipe-audio"

xmobarPipeBluetooth :: FilePath
xmobarPipeBluetooth = "/home/jirik/.xmonad/xmobar-pipe-bluetooth"


myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth :: Dimension
myBorderWidth = 2

myModMask :: KeyMask
myModMask = mod4Mask

myWorkspaces :: [String]
myWorkspaces = ["1","2","3","4","5","6","7","8","9","0","A","B"]

-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

createPipe :: (MonadIO m) => FilePath -> m ()
createPipe path = spawn $ printf "test -p \"%s\" || mkfifo \"%s\"" path path

constantToFile :: (MonadIO m, Show a) => a -> FilePath -> m ()
constantToFile a file = spawn $ printf "echo \"%s\" > \"%s\"" (show a) file

-----------------------------------------------------------------------
-- Key bindings helpers
-----------------------------------------------------------------------

xF86XK_AudioMicMute ::KeySym
xF86XK_AudioMicMute = 0x1008ffb2

nextMatchOrDoForward :: Query Bool -> String -> X ()
nextMatchOrDoForward p s = nextMatchOrDo Forward p (spawn s)

nextMatchOrDoForwardClass :: String -> String -> X ()
nextMatchOrDoForwardClass c = nextMatchOrDoForward (className =? c)

contains :: (Functor f, Eq a) => f [a] -> [a] -> f Bool
contains q x = fmap (List.isInfixOf x) q

nextMatchOrDoForwardTitleContains :: String -> String -> X ()
nextMatchOrDoForwardTitleContains t = nextMatchOrDoForward (title `contains` t)

followTo :: Direction1D -> WSType -> X ()
followTo dir t = doTo dir t getSortByIndex (\w -> windows (StackSet.shift w) >> windows (StackSet.greedyView w))

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
------------------------------------------------------------------------

myKeys :: XConfig Layout -> Map.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modm} = Map.fromList $

  [ ((modm ,              xK_x     ), nextMatchOrDoForwardClass myTerminalClass myTerminal)

  , ((modm .|. shiftMask, xK_x     ), spawn myTerminal)

  , ((modm ,              xK_f     ), nextMatchOrDoForwardClass "Firefox" "firefox")

  , ((modm .|. shiftMask, xK_f     ), spawn "firefox")

  , ((modm ,              xK_g     ), nextMatchOrDoForwardClass "Thunderbird" "thunderbird")

  , ((modm ,              xK_z     ), shellPrompt myXPConfig)

  , ((modm ,              xK_a     ), nextMatchOrDoForwardClass "Pcmanfm" "pcmanfm")

  , ((modm .|. shiftMask, xK_a     ), spawn "pcmanfm")

  , ((modm ,              xK_Escape), spawn "lock-now")

  , ((modm .|. shiftMask, xK_Escape), spawn "suspend-now")

  , ((modm ,              xK_s     ), nextMatchOrDoForwardTitleContains "safe.kdb" "keepassx-safe")

  , ((modm .|. shiftMask, xK_s     ), nextMatchOrDoForwardTitleContains "safe-secure.kdb" "keepassx-safe-secure")

  , ((modm ,              xK_r     ), nextMatchOrDoForwardClass "jetbrains-idea-ce" "idea")

  , ((modm .|. shiftMask, xK_r     ), spawn "idea")

  , ((modm ,              xK_v     ), nextMatchOrDoForwardClass "Vlc" "nlvlc")

  , ((0 ,  xF86XK_AudioRaiseVolume ), spawn "pa-volume-up")

  , ((0 ,  xF86XK_AudioLowerVolume ), spawn "pa-volume-down")

  , ((0 ,  xF86XK_AudioMute        ), spawn "pa-volume-mute")

  , ((modm , xF86XK_AudioRaiseVolume ), spawn "pa-mic-up")

  , ((modm , xF86XK_AudioLowerVolume ), spawn "pa-mic-down")

  , ((0 ,  xF86XK_AudioMicMute     ), spawn "pa-mic-mute")

  , ((0 ,  xK_Print                ), spawn "screenshot")

  , ((modm ,               xK_BackSpace ), spawn "screenshot")

  , ((modm .|. shiftMask , xK_BackSpace ), spawn "screenshot-window")

  , ((0 , xF86XK_MonBrightnessDown ), spawn ("xbacklight -" ++ myBrightnessStep ))

  , ((0 , xF86XK_MonBrightnessUp   ), spawn ("xbacklight +" ++ myBrightnessStep ))

  , ((shiftMask, xF86XK_MonBrightnessDown ), spawn ("xbacklight -set " ++ myBrightnessDefaultLow ))

  , ((shiftMask, xF86XK_MonBrightnessUp   ), spawn ("xbacklight -set " ++ myBrightnessDefaultHigh ))

  -- close focused window
  , ((modm ,              xK_q     ), kill)
  , ((modm .|. shiftMask, xK_q     ), kill)

   -- Rotate through the available layout algorithms
  , ((modm ,              xK_space ), sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default
  , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size
  , ((modm ,              xK_n     ), refresh)

  -- Cycle Screens
  , ((modm ,              xK_w     ), nextScreen)

  -- Move to next Screen
  , ((modm .|. shiftMask, xK_w     ), shiftNextScreen)

  -- Move focus to the next window
  , ((modm ,              xK_o     ), windows StackSet.focusDown)

  -- Move focus to the previous window
  , ((modm ,              xK_p     ), windows StackSet.focusUp  )

  -- Move focus to the master window
  , ((modm ,              xK_m     ), windows StackSet.focusMaster  )

  -- Swap the focused window and the master window
  , ((modm ,              xK_Return), windows StackSet.swapMaster)

  -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_o     ), windows StackSet.swapDown  )

  -- Swap the focused window with the previous window
  , ((modm .|. shiftMask, xK_p     ), windows StackSet.swapUp    )

  -- Shrink the master area
  , ((modm ,              xK_h     ), sendMessage Shrink)

  -- Expand the master area
  , ((modm ,              xK_l     ), sendMessage Expand)

  -- Push window back into tiling
  , ((modm ,              xK_t     ), withFocused $ windows . StackSet.sink)

  -- Increment the number of windows in the master area
  , ((modm              , xK_period), sendMessage (IncMasterN 1))

  -- Deincrement the number of windows in the master area
  , ((modm              , xK_comma ), sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap
  , ((modm              , xK_b     ), sendMessage ToggleStruts)

  -- Go to empty workspace
  , ((modm              , xK_grave ), moveTo Next EmptyWS)

  -- Shift window to empty workspace
  , ((modm .|. shiftMask, xK_grave ), followTo Next EmptyWS)

  -- Focus previous window
  , ((modm              , xK_Right ), nextMatch Forward (return True))

  -- Focus next window
  , ((modm              , xK_Left  ), nextMatch Backward (return True))

  -- Go to previous window
  , ((modm              , xK_Tab   ), nextMatch History (return True))

  , ((shiftMask         , xK_Escape), spawn "xdotool key Caps_Lock")

  ]
  ++
  -- workspace switching
  [((m .|. modm , k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal])
    , (f, m) <- [(StackSet.greedyView, 0), (StackSet.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
------------------------------------------------------------------------

myMouseBindings :: XConfig t -> Map.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = Map.fromList

  -- mod-button1, Set the window to floating mode and move by dragging
  [ ((modm , button1), \w -> focus w >> mouseMoveWindow w
                                     >> windows StackSet.shiftMaster)

  -- mod-button2, Raise the window to the top of the stack
  , ((modm , button2), \w -> focus w >> windows StackSet.shiftMaster)

  -- mod-button3, Set the window to floating mode and resize by dragging
  , ((modm , button3), \w -> focus w >> mouseResizeWindow w
                                     >> windows StackSet.shiftMaster)
  ]

------------------------------------------------------------------------
-- Layouts:
------------------------------------------------------------------------

myLayout = avoidStruts layout
  where
    layout  = smartBorders tiled
           ||| smartBorders (Mirror tiled)
           ||| noBorders Full
           ||| smartBorders Grid
    tiled = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

------------------------------------------------------------------------
-- Window rules:
------------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = manageDocks <+> composeAll
  [ isDialog --> doCenterFloat
  ]

------------------------------------------------------------------------
-- Event handling
------------------------------------------------------------------------

myEventHook = mempty <+> docksEventHook <+> fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging
------------------------------------------------------------------------

myLogHook xmobar = 
  dynamicLogWithPP (def
    { ppOutput = hPutStrLn xmobar
    , ppTitle = xmobarColor myXmobarFgColor "" . shorten 110
    , ppCurrent = xmobarColor myXmobarColorRed myXmobarHiColor . pad
    , ppHidden = pad
    , ppSep = xmobarColor myXmobarHiColor "" " | "
    , ppWsSep = ""
    , ppLayout = \x -> case x of "Tall"        -> "T"
                                 "Mirror Tall" -> "M"
                                 "Full"        -> "F"
                                 "Grid"        -> "G"
                                 _ -> x
    })
    >> historyHook
    >> updatePointer (0.5, 0.5) (0, 0)

------------------------------------------------------------------------
-- Startup hook
------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook =
     ewmhDesktopsStartup
  >> setWMName "LG3D"
  >> constantToFile myXmobarColorGrn "${HOME}/.xmonad/xmobar-color-grn"
  >> constantToFile myXmobarColorRed "${HOME}/.xmonad/xmobar-color-red"
  >> createPipe xmobarPipeAudio
  >> spawn "do-every 10 xmobar-update-audio-status"
  >> createPipe xmobarPipeBluetooth
  >> spawn "do-every 10 xmobar-update-bluetooth-status"

------------------------------------------------------------------------
-- Config
------------------------------------------------------------------------

myXPConfig :: XPConfig
myXPConfig =
  def { font              = myXFTFont
      , bgColor           = myXmobarBgColor
      , fgColor           = myXmobarFgColor
      , bgHLight          = myXmobarColorRed
      , fgHLight          = myXmobarBgColor
      , borderColor       = myFocusedBorderColor
      , promptBorderWidth = 1
      , height            = 16
      , position          = Bottom
      , historySize       = 100
      , historyFilter     = deleteConsecutive
      }

------------------------------------------------------------------------
-- Xmobar configuration
------------------------------------------------------------------------

xmobarComParameters :: [String] -> String
xmobarComParameters [] = " [] "
xmobarComParameters c  = " [\"" ++ List.intercalate "\",\"" c ++ "\"] "

xmobarLook :: String
xmobarLook = concat
  [ " --font=\"", myXFTFont, "\""
  , " --bgcolor=\"", myXmobarBgColor, "\""
  , " --fgcolor=\"", myXmobarFgColor, "\""
  ]

xmobarStdin :: String
xmobarStdin =
  "Run StdinReader"

xmobarCommand :: String -> [String] -> String -> Integer -> String
xmobarCommand c p = printf "Run Com \"%s\" %s \"%s\" %d" c (xmobarComParameters p)

xmobarLoad :: Integer -> String -> Integer -> String
xmobarLoad c = xmobarCommand "xmobar-load-status" [ myXmobarColorRed, myXmobarColorGrn, show c ]

xmobarNetwork :: String -> String -> String -> Integer -> String
xmobarNetwork lan wlan = xmobarCommand "xmobar-net-status" [ myXmobarColorRed, myXmobarColorGrn, lan, wlan ]

xmobarBattery :: Integer -> String
xmobarBattery rr = concat
  [ "Run BatteryP"
  , xmobarComParameters [ "BAT0", "BAT1" ]
  , xmobarComParameters [ "--template", "<acstatus> : <left>% : <timeleft>h"
                        , "--Low"     , "10"
                        , "--High"    , "80"
                        , "--low"     , myXmobarColorRed
                        , "--normal"  , myXmobarFgColor
                        , "--high"    , myXmobarColorGrn
                        , "--"
                        , "-o", "<fc=" ++ myXmobarColorRed  ++ ">D</fc>"
                        , "-O", "<fc=" ++ myXmobarColorGrn ++ ">C</fc>"
                        , "-i", "<fc=" ++ myXmobarFgColor   ++ ">F</fc>"
                        ]
  , show rr
  ]

xmobarMemory :: Integer -> String
xmobarMemory rr = concat
  [ "Run Memory"
  , xmobarComParameters [ "--template", "M : <usedratio>%"
                        , "--Low"     , "20"
                        , "--High"    , "90"
                        , "--low"     , myXmobarColorGrn
                        , "--normal"  , myXmobarFgColor
                        , "--high"    , myXmobarColorRed
                        ]
  , show rr
  ]

xmobarDate :: Integer -> String
xmobarDate rr = "Run Date \"%H:%M:%S\" \"date\" " ++ show rr

xmobarCommands :: [String] -> String
xmobarCommands c = " --commands=\'[" ++ List.intercalate ", " c ++ "]\'"

xmobarPipe :: String -> String -> String
xmobarPipe f a = "Run PipeReader \"" ++ f ++ "\" \"" ++ a ++ "\""

xmobarSep :: String
xmobarSep = "<fc=" ++ myXmobarHiColor ++ ">|</fc>"

xmobarTemplate :: String -> String
xmobarTemplate "eos" =
  xmobarCommands [ xmobarStdin
                 , xmobarLoad 4 "load" 100
                 , xmobarMemory 100
                 , xmobarNetwork "enp4s0" "" "network" 600
                 , xmobarPipe xmobarPipeBluetooth "bluetooth"
                 , xmobarPipe xmobarPipeAudio "audio"
                 , xmobarDate 10
                 ]
  ++ List.concat [ " -t \'%StdinReader%}{ " , xmobarSep
                 , " %load% ", xmobarSep
                 , " %memory% ", xmobarSep
                 , " %network% ", xmobarSep
                 , " %bluetooth% ", xmobarSep
                 , " %audio% ", xmobarSep
                 , " %date% "
                 , "\'"
                 ]
xmobarTemplate "eurus" =
  xmobarCommands [ xmobarStdin
                 , xmobarLoad 4 "load" 100
                 , xmobarMemory 100
                 , xmobarNetwork "enp0s25" "wlp3s0" "network" 600
                 , xmobarPipe xmobarPipeBluetooth "bluetooth"
                 , xmobarPipe xmobarPipeAudio "audio"
                 , xmobarBattery 600
                 , xmobarDate 10
                 ]
  ++ List.concat [ " -t \'%StdinReader%}{ " , xmobarSep
                 , " %load% ", xmobarSep
                 , " %memory% ", xmobarSep
                 , " %network% ", xmobarSep
                 , " %bluetooth% ", xmobarSep
                 , " %audio% ", xmobarSep
                 , " %battery% ", xmobarSep
                 , " %date% "
                 , "\'"
                 ]
xmobarTemplate "notus" =
  xmobarCommands [ xmobarStdin
                 , xmobarLoad 4 "load" 100
                 , xmobarMemory 100
                 , xmobarNetwork "enp0s25" "wls3" "network" 600
                 , xmobarPipe xmobarPipeAudio "audio"
                 , xmobarBattery 600
                 , xmobarDate 10
                 ]
  ++ List.concat [ " -t \'%StdinReader%}{ " , xmobarSep
                 , " %load% ", xmobarSep
                 , " %memory% ", xmobarSep
                 , " %network% ", xmobarSep
                 , " %audio% ", xmobarSep
                 , " %battery% ", xmobarSep
                 , " %date% "
                 , "\'"
                 ]

xmobarTemplate _ = ""

xmobarParameters :: String -> String
xmobarParameters h = xmobarLook ++ xmobarTemplate h

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
  hostnm <- getHostName
  let shortHostnm = takeWhile (/= '.') hostnm
  xmobar <- spawnPipe ("xmobar" ++ xmobarParameters shortHostnm)
  _ <- spawn "xmobar-update-audio-status"
  _ <- spawn "xmobar-update-bluetooth-status"

  xmonad $ docks $ def
    { terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    -- key bindings
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    -- hooks, layouts
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , handleEventHook    = myEventHook
    , logHook            = myLogHook xmobar
    , startupHook        = myStartupHook
    }
