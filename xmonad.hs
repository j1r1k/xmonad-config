import Control.Monad (mapM)
import Data.Foldable (traverse_)
import qualified Data.List as List (concat, intercalate, isInfixOf)
import qualified Data.Map as Map (Map, fromList)
import qualified Data.Monoid as Monoid (All (..))
import GHC.IO.Handle.Types (Handle)
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioMute, xF86XK_AudioRaiseVolume, xF86XK_Bluetooth, xF86XK_Display, xF86XK_LaunchA, xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp)
import System.Exit ()
import System.FilePath.Posix ((</>))
import System.IO (hPutStrLn)
import System.ReadEnvVar (lookupEnvEx)
import Text.Printf (printf)
import XMonad
import XMonad.Actions.CycleWS (WSType (EmptyWS), doTo, moveTo, nextScreen, shiftNextScreen, swapNextScreen)
import XMonad.Actions.GroupNavigation (Direction (Backward, Forward, History), historyHook, nextMatch, nextMatchOrDo)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, pad, ppCurrent, ppHidden, ppLayout, ppOutput, ppSep, ppTitle, ppWsSep, shorten, xmobarColor)
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsStartup, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (AvoidStruts, ToggleStruts (ToggleStruts), avoidStruts, docks, docksEventHook, manageDocks)
import XMonad.Hooks.ManageHelpers (doFloatDep, doFullFloat, isDialog, isFullscreen)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.Grid (Grid (Grid))
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.NoBorders (SmartBorder, WithBorder, noBorders, smartBorders)
import XMonad.Prompt (Direction1D (Next), XPConfig (..), XPPosition (..), deleteConsecutive)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.StackSet (RationalRect (..))
import qualified XMonad.StackSet as StackSet (focusDown, focusUp, greedyView, shift, shiftMaster, sink, swapDown, swapMaster, swapUp)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

type Hostname = String

type Home = FilePath

type XmonadHome = FilePath

-----------------------------------------------------------------------
-- Customizations
-----------------------------------------------------------------------

myTerminal :: String
myTerminal = "terminal-tmux"

myTerminalClass :: String
myTerminalClass = "URxvt"

myBrightnessStep :: String
myBrightnessStep = "5"

myBrightnessDefaultLow :: String
myBrightnessDefaultLow = "15"

myBrightnessDefaultHigh :: String
myBrightnessDefaultHigh = "80"

myNormalBorderColor :: String
myNormalBorderColor = "#dcdccc"

myFocusedBorderColor :: String
myFocusedBorderColor = "#de7168"

myXFTFont :: String
myXFTFont = "xft:DejaVu Sans Mono-10:antialias=true"

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

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth :: Dimension
myBorderWidth = 2

myModMask :: KeyMask
myModMask = mod4Mask

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "A", "B"]

xmonadHome :: Home -> XmonadHome
xmonadHome home = home </> ".xmonad"

xmobarPipeAudio :: XmonadHome -> FilePath
xmobarPipeAudio xHome = xHome </> "xmobar-pipe-audio"

xmobarPipeBluetooth :: XmonadHome -> FilePath
xmobarPipeBluetooth xHome = xHome </> "xmobar-pipe-bluetooth"

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

xF86XK_AudioMicMute :: KeySym
xF86XK_AudioMicMute = 0x1008ffb2

nextMatchClass :: String -> X ()
nextMatchClass c = nextMatch Forward (className =? c)

nextMatchOrSpawn :: Query Bool -> String -> X ()
nextMatchOrSpawn p s = nextMatchOrDo Forward p (spawn s)

nextMatchClassOrSpawn :: String -> String -> X ()
nextMatchClassOrSpawn c = nextMatchOrSpawn (className =? c)

contains :: (Functor f, Eq a) => f [a] -> [a] -> f Bool
contains q x = fmap (List.isInfixOf x) q

nextMatchTitleContainsOrSpawn :: String -> String -> X ()
nextMatchTitleContainsOrSpawn t = nextMatchOrSpawn (title `contains` t)

followTo :: Direction1D -> WSType -> X ()
followTo dir t = doTo dir t getSortByIndex (\w -> windows (StackSet.shift w) >> windows (StackSet.greedyView w))

------------------------------------------------------------------------
-- Key bindings
------------------------------------------------------------------------

myKeys :: Hostname -> XConfig Layout -> Map.Map (KeyMask, KeySym) (X ())
myKeys hostname conf@XConfig {XMonad.modMask = modm} =
  Map.fromList $
    [ ((modm, xK_x), nextMatchClassOrSpawn myTerminalClass myTerminal),
      ((modm .|. shiftMask, xK_x), spawn myTerminal),
      ((modm, xK_f), nextMatchClass "Firefox"),
      ((modm .|. shiftMask, xK_f), spawn "firefox"),
      ((modm, xK_c), nextMatchClass "Google-chrome"),
      ((modm .|. shiftMask, xK_c), spawn "google-chrome"),
      ((modm, xK_g), nextMatchClass "Thunderbird"),
      ((modm .|. shiftMask, xK_g), spawn "thunderbird"),
      ((modm, xK_d), nextMatchClass "Slack"),
      ((modm .|. shiftMask, xK_d), spawn "slack"),
      ((modm, xK_a), nextMatchClassOrSpawn "Pcmanfm" "pcmanfm"),
      ((modm .|. shiftMask, xK_a), spawn "pcmanfm"),
      ((modm, xK_s), nextMatchTitleContainsOrSpawn "safe-common" "keepass-safe-common"),
      ((modm .|. shiftMask, xK_s), nextMatchTitleContainsOrSpawn "safe-secure" "keepass-safe-secure"),
      ((modm, xK_e), nextMatchClass "Code"),
      ((modm .|. shiftMask, xK_e), spawn "code"),
      ((modm, xK_r), nextMatchClass "jetbrains-idea"),
      ((modm .|. shiftMask, xK_r), spawn "idea"),
      ((modm, xK_v), nextMatchClass "vlc"),
      ((modm .|. shiftMask, xK_v), spawn "vlc"),
      -- lock & suspend
      ((modm, xK_Escape), spawn "lock-now"),
      ((modm .|. shiftMask, xK_Escape), spawn "suspend-now"),
      -- volume control
      ((0, xF86XK_AudioRaiseVolume), spawn "pa-volume-up 5"),
      ((shiftMask, xF86XK_AudioRaiseVolume), spawn "pa-volume-up 1"),
      ((0, xF86XK_AudioLowerVolume), spawn "pa-volume-down 5"),
      ((shiftMask, xF86XK_AudioLowerVolume), spawn "pa-volume-down 1"),
      ((0, xF86XK_AudioMute), spawn "pa-volume-mute"),
      -- mic volume control
      ((modm, xF86XK_AudioRaiseVolume), spawn "pa-mic-up 5"),
      ((modm, xF86XK_AudioLowerVolume), spawn "pa-mic-down 5"),
      ((0, xF86XK_AudioMicMute), spawn "pa-mic-mute"),
      ((0, xF86XK_Display), spawn "cycle"),
      ((0, xF86XK_LaunchA), spawn "cycle"),
      -- screenshots
      ((0, xK_Print), spawn "screenshot"),
      ((modm, xK_BackSpace), spawn "screenshot"),
      ((modm .|. shiftMask, xK_BackSpace), spawn "screenshot-select"),
      -- backlight controls
      ((0, xF86XK_MonBrightnessDown), spawn ("xbacklight -" ++ myBrightnessStep)),
      ((0, xF86XK_MonBrightnessUp), spawn ("xbacklight +" ++ myBrightnessStep)),
      ((shiftMask, xF86XK_MonBrightnessDown), spawn ("xbacklight -set " ++ myBrightnessDefaultLow)),
      ((shiftMask, xF86XK_MonBrightnessUp), spawn ("xbacklight -set " ++ myBrightnessDefaultHigh)),
      -- bluetooth
      ((0, xF86XK_Bluetooth), spawn "bt-toggle"),
      -- show prompt
      ((modm, xK_z), shellPrompt $ myXPConfig hostname),
      -- close focused window
      ((modm, xK_q), kill),
      ((modm .|. shiftMask, xK_q), kill),
      -- rotate through the available layout algorithms
      ((modm, xK_space), sendMessage NextLayout),
      -- reset the layouts on the current workspace to default
      ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
      -- resize viewed windows to the correct size
      ((modm, xK_n), refresh),
      -- cycle Screens
      ((modm, xK_w), nextScreen),
      -- move to next Screen
      ((modm .|. shiftMask, xK_w), shiftNextScreen),
      -- swap screens
      ((modm, xK_m), swapNextScreen),
      -- move focus to the next window
      ((modm, xK_o), windows StackSet.focusDown),
      -- move focus to the previous window
      ((modm, xK_p), windows StackSet.focusUp),
      -- swap the focused window and the master window
      ((modm, xK_Return), windows StackSet.swapMaster),
      -- swap the focused window with the next window
      ((modm .|. shiftMask, xK_o), windows StackSet.swapDown),
      -- swap the focused window with the previous window
      ((modm .|. shiftMask, xK_p), windows StackSet.swapUp),
      -- shrink the master area
      ((modm, xK_h), sendMessage Shrink),
      -- expand the master area
      ((modm, xK_l), sendMessage Expand),
      -- push window back into tiling
      ((modm, xK_t), withFocused $ windows . StackSet.sink),
      -- increment the number of windows in the master area
      ((modm, xK_period), sendMessage $ IncMasterN 1),
      -- deincrement the number of windows in the master area
      ((modm, xK_comma), sendMessage $ IncMasterN (-1)),
      -- toggle the status bar gap
      ((modm, xK_b), sendMessage ToggleStruts),
      -- go to empty workspace
      ((modm, xK_grave), moveTo Next EmptyWS),
      -- shift window to empty workspace
      ((modm .|. shiftMask, xK_grave), followTo Next EmptyWS),
      -- focus previous window
      ((modm, xK_Right), nextMatch Forward (return True)),
      -- focus next window
      ((modm, xK_Left), nextMatch Backward (return True)),
      -- go to previous window
      ((modm, xK_Tab), nextMatch History (return True)),
      -- caps lock remap
      ((shiftMask, xK_Escape), spawn "xdotool key Caps_Lock")
    ]
      ++
      -- workspace switching
      [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal]),
          (f, m) <- [(StackSet.greedyView, 0), (StackSet.shift, shiftMask)]
      ]

------------------------------------------------------------------------
-- Mouse bindings
------------------------------------------------------------------------

myMouseBindings :: XConfig t -> Map.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} =
  Map.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1),
        \w ->
          focus w >> mouseMoveWindow w
            >> windows StackSet.shiftMaster
      ),
      -- mod-button2, Raise the window to the top of the stack
      ((modm, button2), \w -> focus w >> windows StackSet.shiftMaster),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (modm, button3),
        \w ->
          focus w >> mouseResizeWindow w
            >> windows StackSet.shiftMaster
      )
    ]

------------------------------------------------------------------------
-- Layout
------------------------------------------------------------------------

myLayout ::
  ModifiedLayout AvoidStruts
    ( Choose (ModifiedLayout SmartBorder Tall)
        ( Choose (ModifiedLayout SmartBorder (Mirror Tall))
            ( Choose (ModifiedLayout WithBorder Full)
                (ModifiedLayout SmartBorder Grid)
            )
        )
    )
    Window
myLayout = avoidStruts layout
  where
    layout =
      smartBorders tiled
        ||| smartBorders (Mirror tiled)
        ||| noBorders Full
        ||| smartBorders Grid
    tiled = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2
    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100

------------------------------------------------------------------------
-- Manage hook
------------------------------------------------------------------------

fitRectSizeIn :: (Fractional n, Ord n) => n -> n -> (n, n)
fitRectSizeIn limit size = let n = min limit size in ((1 - n) / 2, n)

computeDialogRect :: RationalRect -> RationalRect
computeDialogRect (RationalRect _ _ w h) =
  let limit = 0.6
      (x', w') = fitRectSizeIn limit w
      (y', h') = fitRectSizeIn limit h
   in RationalRect x' y' w' h'

myManageHook :: ManageHook
myManageHook =
  manageDocks
    <+> composeAll
      [ isDialog --> doFloatDep computeDialogRect,
        className =? "Peek" --> doFloatDep computeDialogRect,
        isFullscreen --> doFullFloat
      ]

------------------------------------------------------------------------
-- Event hook
------------------------------------------------------------------------

myEventHook :: Event -> X Monoid.All
myEventHook = mempty <+> docksEventHook <+> fullscreenEventHook

------------------------------------------------------------------------
-- Log hook
------------------------------------------------------------------------

xmobarOutputs :: [Handle] -> String -> IO ()
xmobarOutputs xmobarHandles input = traverse_ (flip hPutStrLn input) xmobarHandles

myLogHook :: [Handle] -> X ()
myLogHook xmobarHandles =
  dynamicLogWithPP
    ( def
        { ppOutput = xmobarOutputs xmobarHandles,
          ppTitle = xmobarColor myXmobarFgColor "" . shorten 110,
          ppCurrent = xmobarColor myXmobarColorRed myXmobarHiColor . pad,
          ppHidden = pad,
          ppSep = xmobarColor myXmobarHiColor "" " | ",
          ppWsSep = "",
          ppLayout = \x -> case x of
            "Tall" -> "T"
            "Mirror Tall" -> "M"
            "Full" -> "F"
            "Grid" -> "G"
            _ -> x
        }
    )
    >> historyHook
    >> updatePointer (0.5, 0.5) (0, 0)

------------------------------------------------------------------------
-- Startup hook
------------------------------------------------------------------------

myStartupHook :: XmonadHome -> X ()
myStartupHook xHome =
  ewmhDesktopsStartup
    >> setWMName "LG3D"
    >> constantToFile myXmobarColorGrn (xHome </> "xmobar-color-grn")
    >> constantToFile myXmobarColorRed (xHome </> "xmobar-color-red")
    >> createPipe (xmobarPipeAudio xHome)
    >> createPipe (xmobarPipeBluetooth xHome)

------------------------------------------------------------------------
-- Prompt config
------------------------------------------------------------------------

myXPConfig :: Hostname -> XPConfig
myXPConfig hostname =
  def
    { font = myXFTFont,
      bgColor = myXmobarBgColor,
      fgColor = myXmobarFgColor,
      bgHLight = myXmobarColorRed,
      fgHLight = myXmobarBgColor,
      borderColor = myFocusedBorderColor,
      promptBorderWidth = 1,
      height = case hostname of
        "eurus" -> 16
        _ -> 24,
      position = Bottom,
      historySize = 100,
      historyFilter = deleteConsecutive
    }

------------------------------------------------------------------------
-- Xmobar configuration
------------------------------------------------------------------------

xmobarComParameters :: [String] -> String
xmobarComParameters [] = " [] "
xmobarComParameters c = " [\"" ++ List.intercalate "\",\"" c ++ "\"] "

xmobarLook :: String
xmobarLook =
  concat
    [ " --font=\"",
      myXFTFont,
      "\"",
      " --bgcolor=\"",
      myXmobarBgColor,
      "\"",
      " --fgcolor=\"",
      myXmobarFgColor,
      "\""
    ]

xmobarStdin :: String
xmobarStdin =
  "Run StdinReader"

xmobarCommand :: String -> [String] -> String -> Integer -> String
xmobarCommand c p = printf "Run Com \"%s\" %s \"%s\" %d" c (xmobarComParameters p)

xmobarLoad :: String -> Integer -> String
xmobarLoad = xmobarCommand "xmobar-load-status" [myXmobarColorRed, myXmobarColorGrn]

xmobarNetwork :: String -> String -> String -> Integer -> String
xmobarNetwork lan wlan = xmobarCommand "xmobar-net-status" [myXmobarColorRed, myXmobarColorGrn, lan, wlan]

xmobarBattery :: Integer -> String
xmobarBattery rr =
  concat
    [ "Run BatteryP",
      xmobarComParameters ["BAT0", "BAT1"],
      xmobarComParameters
        [ "--template",
          "<acstatus> : <left>% : <timeleft>h",
          "--Low",
          "10",
          "--High",
          "80",
          "--low",
          myXmobarColorRed,
          "--normal",
          myXmobarFgColor,
          "--high",
          myXmobarColorGrn,
          "--",
          "-o",
          "<fc=" ++ myXmobarColorRed ++ ">D</fc>",
          "-O",
          "<fc=" ++ myXmobarColorGrn ++ ">C</fc>",
          "-i",
          "<fc=" ++ myXmobarFgColor ++ ">F</fc>"
        ],
      show rr
    ]

xmobarMemory :: Integer -> String
xmobarMemory rr =
  concat
    [ "Run Memory",
      xmobarComParameters
        [ "--template",
          "M : <usedratio>%",
          "--Low",
          "20",
          "--High",
          "90",
          "--low",
          myXmobarColorGrn,
          "--normal",
          myXmobarFgColor,
          "--high",
          myXmobarColorRed
        ],
      show rr
    ]

xmobarDate :: Integer -> String
xmobarDate rr = "Run Date \"%H:%M:%S\" \"date\" " ++ show rr

xmobarCommands :: [String] -> String
xmobarCommands c = " --commands=\'[" ++ List.intercalate ", " c ++ "]\'"

xmobarPipe :: String -> String -> String
xmobarPipe f a = "Run PipeReader \"" ++ f ++ "\" \"" ++ a ++ "\""

xmobarSep :: String
xmobarSep = "<fc=" ++ myXmobarHiColor ++ ">|</fc>"

xmobarTemplate :: Home -> Hostname -> String
xmobarTemplate home "eos" =
  xmobarCommands
    [ xmobarStdin,
      xmobarLoad "load" 100,
      xmobarMemory 100,
      xmobarNetwork "enp2s0" "wlp6s0" "network" 600,
      xmobarPipe (xmobarPipeBluetooth home) "bluetooth",
      xmobarPipe (xmobarPipeAudio home) "audio",
      xmobarDate 10
    ]
    ++ List.concat
      [ " -t \'%StdinReader%}{ ",
        xmobarSep,
        " %load% ",
        xmobarSep,
        " %memory% ",
        xmobarSep,
        " %network% ",
        xmobarSep,
        " %bluetooth% ",
        xmobarSep,
        " %audio% ",
        xmobarSep,
        " %date% ",
        "\'"
      ]
xmobarTemplate home _ =
  xmobarCommands
    [ xmobarStdin,
      xmobarLoad "load" 100,
      xmobarMemory 100,
      xmobarNetwork "enp0s25" "wlp3s0" "network" 600,
      xmobarPipe (xmobarPipeBluetooth home) "bluetooth",
      xmobarPipe (xmobarPipeAudio home) "audio",
      xmobarBattery 600,
      xmobarDate 10
    ]
    ++ List.concat
      [ " -t \'%StdinReader%}{ ",
        xmobarSep,
        " %load% ",
        xmobarSep,
        " %memory% ",
        xmobarSep,
        " %network% ",
        xmobarSep,
        " %bluetooth% ",
        xmobarSep,
        " %audio% ",
        xmobarSep,
        " %battery% ",
        xmobarSep,
        " %date% ",
        "\'"
      ]

xmobarParameters :: XmonadHome -> Hostname -> Int -> String
xmobarParameters xHome hostname screen =
  List.concat
    [ xmobarLook,
      xmobarTemplate xHome hostname,
      " --screen=" ++ show screen
    ]

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

getHostname :: IO Hostname
getHostname = lookupEnvEx "HOSTNAME"

getHome :: IO Home
getHome = lookupEnvEx "HOME"

main :: IO ()
main = do
  hostname <- getHostname
  let shortHostname = takeWhile (/= '.') hostname
  home <- getHome
  let xHome = xmonadHome home
  nScreens <- countScreens
  xmobarHandles <- mapM (\screen -> spawnPipe $ "xmobar" ++ xmobarParameters xHome shortHostname screen) $ enumFromTo 0 (nScreens - 1)
  xmonad $ docks $
    def
      { terminal = myTerminal,
        focusFollowsMouse = myFocusFollowsMouse,
        borderWidth = myBorderWidth,
        modMask = myModMask,
        workspaces = myWorkspaces,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        -- key bindings
        keys = myKeys shortHostname,
        mouseBindings = myMouseBindings,
        -- hooks, layouts
        layoutHook = myLayout,
        manageHook = myManageHook,
        handleEventHook = myEventHook,
        logHook = myLogHook xmobarHandles,
        startupHook = myStartupHook xHome
      }
