import Control.Monad (filterM)

import Data.Char (toLower)
import qualified Data.List as List (concat, intercalate, isInfixOf)
import qualified Data.Map as Map (fromList, lookup, toList, Map)

import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioMute, xF86XK_AudioRaiseVolume, xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp)

import Network.HostName (getHostName)

import System.Exit ()
import System.IO (hPutStrLn)

import Text.Printf (printf)
import Text.Regex.Posix

import XMonad 
import XMonad.Actions.CycleWS (doTo, moveTo, nextScreen, shiftNextScreen, WSType(EmptyWS))
import XMonad.Actions.GroupNavigation (historyHook, nextMatch, nextMatchOrDo, Direction(Backward, Forward, History))
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.WindowGo ()
import XMonad.Hooks.DynamicLog (defaultPP, dynamicLogWithPP, pad, ppCurrent, ppHidden, ppLayout, ppOutput, ppSep, ppTitle, ppWsSep, shorten, xmobarColor)
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsStartup)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(ToggleStruts))
import XMonad.Hooks.ManageHelpers (doCenterFloat, isDialog)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Simplest (Simplest(Simplest))
import XMonad.Layout.SubLayouts (subTabbed, GroupMsg(Merge, UnMerge))
import XMonad.Prompt (defaultXPConfig, deleteConsecutive, getNextCompletion, mkXPrompt, Direction1D(Next), XPConfig(..), XPPosition(..), XPrompt(..))
import XMonad.Prompt.Shell (shellPrompt)
import qualified XMonad.StackSet as StackSet (focusDown, focusMaster, focusUp, focusWindow, greedyView, insertUp, index, integrate', shift, shiftMaster, sink, stack, swapDown, swapMaster, swapUp, tag, workspaces)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

-----------------------------------------------------------------------
-- Customizations
-----------------------------------------------------------------------

myTerminal :: String
myTerminal      = "urxvt"
myTerminalClass :: String
myTerminalClass = "URxvt"
myBrowser :: String
myBrowser       = "firefox"
myBrowserClass :: String
myBrowserClass  = "Firefox"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth :: Dimension
myBorderWidth   = 2

myModMask :: KeyMask
myModMask       = mod4Mask
--altMask :: KeyMask
--altMask         = mod1Mask

myWorkspaces :: [String]
myWorkspaces    = ["1","2","3","4","5","6","7","8","9","0","A","B"]

myNormalBorderColor :: String
myNormalBorderColor  = "#dcdccc"

myFocusedBorderColor :: String
myFocusedBorderColor = "#de7168"

myXmobarFgColor :: String
myXmobarFgColor = myNormalBorderColor

myXmobarBgColor :: String
myXmobarBgColor = "#000000"

myXmobarHiColor :: String
myXmobarHiColor = "#575757"

myXmobarCursorColor :: String
myXmobarCursorColor  = "#ff8278"

myXmobarColorBad :: String
myXmobarColorBad = myXmobarCursorColor

myXmobarColorGood :: String
myXmobarColorGood = "#9ec400"

myXFTFont :: String
myXFTFont       = "xft:DejaVu Sans Mono-10:antialias=true"

myBrightnessStep :: String
myBrightnessStep        = "5"
myBrightnessDefaultLow :: String
myBrightnessDefaultLow  = "15"

myBrightnessDefaultHigh :: String
myBrightnessDefaultHigh = "50"

xmobarPipeAudio :: String
xmobarPipeAudio = "/home/jirik/.xmonad/xmobar-pipe-audio"

xmobarPipeBluetooth :: String
xmobarPipeBluetooth = "/home/jirik/.xmonad/xmobar-pipe-bluetooth"

-----------------------------------------------------------------------
-- Key bindings helpers
-----------------------------------------------------------------------

xF86XK_AudioMicMute ::KeySym
xF86XK_AudioMicMute = 0x1008ffb2

--contains :: (Functor f, Eq a) => f [a] -> [a] -> f Bool
--contains q x = fmap (List.isInfixOf x) q

--nextMatchForward :: Query Bool -> X ()
--nextMatchForward = nextMatch Forward

--nextMatchForwardClass :: String -> X ()
--nextMatchForwardClass c = nextMatchForward (className =? c)

--nextMatchForwardClassAndTitle :: String -> String -> X ()
--nextMatchForwardClassAndTitle c t = nextMatchForward ((className =? c) <&&> (title `contains` t))

nextMatchOrDoForward :: Query Bool -> String -> X ()
nextMatchOrDoForward p s = nextMatchOrDo Forward p (spawn s)

--nextMatchOrDoForwardTitle :: String -> String -> X ()
--nextMatchOrDoForwardTitle t = nextMatchOrDoForward (title =? t)

nextMatchOrDoForwardClass :: String -> String -> X ()
nextMatchOrDoForwardClass c = nextMatchOrDoForward (className =? c)

followTo :: Direction1D -> WSType -> X ()
followTo dir t = doTo dir t getSortByIndex (\w -> windows (StackSet.shift w) >> windows (StackSet.greedyView w))

--spawnN :: [String] -> X ()
--spawnN = spawn . List.intercalate "; "

--openInTerminal :: String -> String
--openInTerminal c = "it " ++ c

masterWindow :: XState -> Window
masterWindow = head . take 1 . StackSet.index . windowset

mergeWithMaster :: X ()
mergeWithMaster = withFocused (\f -> sendMessage . Merge f =<< gets masterWindow)

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
------------------------------------------------------------------------

myKeys :: XConfig Layout -> Map.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = Map.fromList $

  [ ((modm ,              xK_x     ), nextMatchOrDoForwardClass myTerminalClass myTerminal)
  
  , ((modm .|. shiftMask, xK_x     ), spawn myTerminal)
  
  , ((modm ,              xK_f     ), nextMatchOrDoForwardClass myBrowserClass myBrowser)

  , ((modm .|. shiftMask, xK_f     ), spawn myBrowser)

  --, ((modm ,              xK_d     ), windowMatchPrompt (className =? myBrowserClass) myXPConfig { alwaysHighlight = True, searchPredicate = hasAllWords } )

  , ((modm ,              xK_g     ), nextMatchOrDoForwardClass "Claws-mail" "claws-mail")

  , ((modm ,              xK_z     ), shellPrompt myXPConfig)

  , ((modm ,              xK_a     ), nextMatchOrDoForwardClass "Pcmanfm" "pcmanfm")

  , ((modm .|. shiftMask, xK_a     ), spawn "pcmanfm")

  , ((modm ,              xK_Escape), spawn "lock-now")

  , ((modm .|. shiftMask, xK_Escape), spawn "suspend-now")

  , ((modm ,              xK_s     ), nextMatchOrDoForwardClass "Keepassx" "keepassx")
  
  , ((modm ,              xK_e     ), nextMatchOrDoForwardClass "Gvim" "gvim")

  , ((modm .|. shiftMask, xK_e     ), spawn "gvim")

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

  -- Pull window from sublayout
  , ((modm              , xK_u     ), withFocused (sendMessage . UnMerge))


  , ((shiftMask         , xK_Escape), spawn "xdotool key Caps_Lock")

  -- Merge window with master
  --, ((modm              , xK_i     ), mergeWithMaster )

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
myMouseBindings (XConfig {XMonad.modMask = modm}) = Map.fromList

  -- mod-button1, Set the window to floating mode and move by dragging
  [ ((modm , button1), \w -> focus w >> mouseMoveWindow w
                                     >> windows StackSet.shiftMaster)

  -- mod-button2, Raise the window to the top of the stack
  , ((modm , button2), \w -> focus w >> windows StackSet.shiftMaster)

  -- mod-button3, Set the window to floating mode and resize by dragging
  , ((modm , button3), \w -> focus w >> mouseResizeWindow w
                                     >> windows StackSet.shiftMaster)

  --, ((mod4Mask, button4), \_ -> windows StackSet.focusUp )

  --, ((mod4Mask, button5), \_ -> windows StackSet.focusDown)
  ]

------------------------------------------------------------------------
-- Layouts:
------------------------------------------------------------------------

myLayout = avoidStruts def
  where
    def  = smartBorders tiled
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

--
-- hook to shift to next empty workspace
--
--doShiftToNextEmptyWS :: Query (Data.Monoid.Endo WindowSet)
--doShiftToNextEmptyWS = liftX (findWorkspace getSortByIndex Next EmptyWS 1) >>= doShift

--
-- Hook to merge new window with master if it matches the query
-- https://wiki.haskell.org/Xmonad/Config_archive/adamvo's_xmonad.hs
--
--doQueryMerge :: Monoid b => Query Bool -> Query b
--doQueryMerge qry = do
--  w <- ask
--  aw <- liftX $ filterM (runQuery qry) =<< gets (take 1 . StackSet.index . windowset)
--  liftX $ windows $ StackSet.insertUp w
--  liftX $ (sendMessage . XMonad.Layout.SubLayouts.Merge w) $ head aw
--  liftX $ windows StackSet.shiftMaster
--  idHook

myManageHook :: ManageHook
myManageHook = manageDocks <+> composeAll
--isDialog <&&> className /=? "Keepassx" --> doCenterFloat <+> doF StackSet.focusDown,
  [ isDialog                     --> doCenterFloat
  --, className =? myBrowserClass  --> doQueryMerge (className =? myBrowserClass)
  ]

------------------------------------------------------------------------
-- Event handling
------------------------------------------------------------------------

myEventHook = mempty <+> docksEventHook

------------------------------------------------------------------------
-- Status bars and logging
------------------------------------------------------------------------

myLogHook xmobar = 
  dynamicLogWithPP (defaultPP
    { ppOutput = hPutStrLn xmobar
    , ppTitle = xmobarColor myXmobarFgColor "" . shorten 110
    , ppCurrent = xmobarColor myXmobarCursorColor myXmobarHiColor . pad
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
  -- trigger audio display update on startup
  >> spawn "do-every 10 xmobar-update-audio-status"
  >> spawn "do-every 10 xmobar-update-bluetooth-status"

------------------------------------------------------------------------
-- Config
------------------------------------------------------------------------

myXPConfig :: XPConfig
myXPConfig =
  defaultXPConfig { font                  = myXFTFont
                  , bgColor               = myXmobarBgColor
                  , fgColor               = myXmobarFgColor
                  , bgHLight              = myXmobarCursorColor
                  , fgHLight              = myXmobarBgColor
                  , borderColor           = myFocusedBorderColor
                  , promptBorderWidth     = 1
                  , height                = 16
                  , position              = Bottom
                  , historySize           = 100
                  , historyFilter         = deleteConsecutive
                  }

--mySearchEngine :: XMonad.Actions.Search.SearchEngine
--mySearchEngine = searchEngine "" ""


------------------------------------------------------------------------
-- Window Prompt which displays only windows that match query
------------------------------------------------------------------------

--hasAllWords :: String -> String -> Bool
--hasAllWords ws xs = all (\w -> w `List.isInfixOf` lxs) $ words lws
--  where lws = map toLower ws
--        lxs = map toLower xs
--
--
---- | Returns the window name as will be listed in dmenu.
----   Lowercased, for your convenience (since dmenu is case-sensitive).
----   Tagged with the workspace ID, to guarantee uniqueness, and to let the user
----   know where he's going.
--decorateName :: WindowSpace -> Window -> X String
--decorateName ws w = do
--  name <- getName w
--  return $ "[" ++ StackSet.tag ws ++ "] " ++ show name
--
----
---- | A map from window names to Windows.
--windowMap :: X (Map.Map String Window)
--windowMap = do
--    ws <- gets windowset
--    Map.fromList <$> concat <$> mapM keyValuePairs (StackSet.workspaces ws)
--  where keyValuePairs ws = mapM (keyValuePair ws) $ StackSet.integrate' (StackSet.stack ws)
--        keyValuePair ws w = flip (,) w <$> decorateName ws w
--
--windowMapMatch :: Query Bool -> X (Map.Map String Window)
--windowMapMatch qry = do
--  w <- windowMap
--  x <- filterM (runQuery qry . snd) $ Map.toList w
--  return $ Map.fromList x
--
--data WindowMatchPrompt = WindowMatchPrompt
--
--instance XPrompt WindowMatchPrompt where
--  showXPrompt _ = "> "
--  commandToComplete _ c = c
--  nextCompletion _ = getNextCompletion
--
--windowMatchPrompt :: Query Bool -> XPConfig -> X ()
--windowMatchPrompt qry c = do
--    a <- fmap action wm
--    w <- wm
--    mkXPrompt WindowMatchPrompt c (compList w) a
--  where
--    winAction a m = flip whenJust (windows . a) . flip Map.lookup m
--    action        = winAction StackSet.focusWindow
--    wm            = windowMapMatch qry
--    compList m s  = return . filter (searchPredicate c s) . map fst . Map.toList $ m

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
xmobarLoad c = xmobarCommand "xmobar-load-status" [ myXmobarColorBad, myXmobarColorGood, show c ]

xmobarBluetooth :: String -> Integer -> String
xmobarBluetooth = xmobarCommand "xmobar-bluetooth-status" [ myXmobarColorBad, myXmobarColorGood ]

--xmobarAudio :: String -> Integer -> String
--xmobarAudio = xmobarCommand "xmobar-audio-status" [ myXmobarColorBad ]

xmobarWicd :: String -> Integer -> String
xmobarWicd = xmobarCommand "xmobar-wicd-status" [ myXmobarColorBad ]

xmobarNetwork :: String -> String -> String -> Integer -> String
xmobarNetwork lan wlan = xmobarCommand "xmobar-net-status" [ myXmobarColorBad, myXmobarColorGood, lan, wlan ]

xmobarBattery :: Integer -> String
xmobarBattery rr = concat
  [ "Run BatteryP"
  , xmobarComParameters [ "BAT0", "BAT1" ]
  , xmobarComParameters [ "--template", "<acstatus> : <left>% : <timeleft>h"
                        , "--Low"     , "10"
                        , "--High"    , "80"
                        , "--low"     , myXmobarColorBad
                        , "--normal"  , myXmobarFgColor
                        , "--high"    , myXmobarColorGood
                        , "--"
                        , "-o", "<fc=" ++ myXmobarColorBad  ++ ">D</fc>"
                        , "-O", "<fc=" ++ myXmobarColorGood ++ ">C</fc>"
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
                        , "--low"     , myXmobarColorGood
                        , "--normal"  , myXmobarFgColor
                        , "--high"    , myXmobarColorBad
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

xmobarTemplate _ = ""

xmobarParameters :: String -> String
xmobarParameters h = xmobarLook ++ xmobarTemplate h

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
  hostnm <- getHostName
  xmobar <- spawnPipe ("xmobar" ++ xmobarParameters hostnm)

  xmonad $ defaultConfig {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,
    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
    -- hooks, layouts
    layoutHook         = myLayout,
    manageHook         = myManageHook,
    handleEventHook    = myEventHook,
    logHook            = myLogHook xmobar,
    startupHook        = myStartupHook
  }
