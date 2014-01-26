import XMonad
import XMonad.Layout
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import System.IO (hPutStrLn)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import qualified Data.Map as M
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import Data.Ratio
import XMonad.Layout.LayoutHints
import XMonad.Layout.Spiral
import XMonad.Hooks.FadeInactive
import XMonad.Util.SpawnOnce
import XMonad.Util.Cursor
import XMonad.Util.Run
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsStartup)
import XMonad.Hooks.SetWMName
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow (copy)
import XMonad.StackSet as W
--import XMonad.Util.WorkspaceScreenshot
import XMonad.Actions.WorkspaceNames
import XMonad.Actions.SpawnOn

myNormalBorderColor = "#ffffff"
myFocusedBorderColor = "#000000"

myManageHook = composeAll [ className =? "emacs" --> doF (W.shift "5:emacs")
                          , className =? "xclock" --> doFloat
                          ]

newManageHook =  myManageHook <+> manageDocks <+> (composeAll . concat $
                [
                  [ className =? i --> doFloat | i <- myClassFloats ]
                ])
  where myClassFloats = [] -- ["ghc"]

myKeys x =
    [ ((modMask x .|. controlMask, xK_p), shellPrompt defaultXPConfig)
    , ((modMask x .|. shiftMask, xK_y), kill)
    , ((modMask x .|. controlMask, xK_e), sendMessage ToggleStruts)
--    , ((modMask x .|. controlMask, xK_i), captureWorkspacesWhen defaultPredicate defaultHook horizontally)
    , ((modMask x .|. controlMask, xK_t), spawn $ xrl ++ " --mode 1024x768 && " ++ nc)
    , ((modMask x .|. controlMask, xK_w), spawn $ xrl ++ " --off && " ++ nc)
    , ((modMask x .|. controlMask, xK_n), spawn $ xrv ++ " --mode 1920x1080 && " ++ nc)
    , ((modMask x .|. controlMask, xK_v), spawn $ xrv ++ " --off && " ++ nc)
    , ((modMask x .|. controlMask, xK_s), spawn $ xrv ++ " --mode 1600x900 && " ++ nc)
    , ((modMask x .|. controlMask, xK_z), spawn "slimlock")
    , ((0, stringToKeysym "XF86AudioLowerVolume"), spawn "ossvol -d 1")
    , ((0, stringToKeysym "XF86AudioRaiseVolume"), spawn "ossvol -i 1")
    , ((0, stringToKeysym "XF86AudioMute"), spawn "ossvol -t")
    , ((modMask x .|. controlMask, xK_comma), spawn "ossvol -d 1")
    , ((modMask x .|. controlMask, xK_period), spawn "ossvol -i 1")
    , ((modMask x .|. controlMask, xK_semicolon), spawn "ossvol -t")
    , ((modMask x .|. controlMask, xK_r), unsafeSpawn "xmonad --recompile && xmonad --restart")
    , ((modMask x .|. controlMask, xK_l), sendMessage Expand)
    , ((modMask x .|. controlMask, xK_h), sendMessage Shrink)
    , ((modMask x .|. controlMask, xK_u), withFocused $ windows . W.sink)
    , ((modMask x .|. controlMask, xK_a), spawn "killall xmobar; xmobar &")
    , ((modMask x .|. controlMask, xK_o), spawn "killall xmobar; xmobar -o ~/.Xmobar/infosmall &")
    ] ++ zip (zip (repeat (modMask x)) workspaceKeys) (map (withNthWorkspace W.greedyView) [0..])
      ++
      zip (zip (repeat (modMask x .|. shiftMask)) workspaceKeys) (map (withNthWorkspace W.shift) [0..])
  where
    nc = "nitrogen --restore &"
    xrl = "xrandr --output LVDS1"
    xrv = "xrandr --output VGA1"

workspaceKeys = [ xK_f, xK_g, xK_c, xK_r, xK_l
                , xK_d, xK_h, xK_t, xK_n, xK_s
                , xK_b, xK_m, xK_w, xK_v, xK_z
                ]

-- Don't change the union order! It overrides default keys.
newKeys x =  M.fromList (myKeys x) `M.union` keys defaultConfig x


myLayoutHook = avoidStruts (tiled ||| Mirror tiled ||| Circle ||| Full)
    where
      tiled = Tall nmaster delta ratio
      nmaster = 1
      delta = 3/100
      ratio = 1/2

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
     where fadeAmount = 0.55

myWorkspaces = [ "1:images", "2:web", "3:irc", "4:music", "5:emacs"
               , "6:rtorrent", "7:mail", "8:compile", "9:volume", "10:monitors"
               , "10:media", "11:pdf", "12:term", "13:misc", "14:trash"
               ]

--spawnOnce ". $HOME/.xmonad/dzen2sto
myStartHook = setDefaultCursor xC_left_ptr <+>
              ewmhDesktopsStartup >> setWMName "LG3D"


main = do
  -- initCapturing -- necessary for xmonad-screenshot
  xmonad $ defaultConfig
             { borderWidth = 0
             , modMask = mod4Mask
             , terminal = "urxvt"
             , manageHook = newManageHook
             , logHook = myLogHook
             , keys = newKeys
             , layoutHook = myLayoutHook
             , handleEventHook = docksEventHook
             , normalBorderColor = myNormalBorderColor
             , focusedBorderColor = myFocusedBorderColor
             -- , startupHook = myStartHook
             , XMonad.workspaces = myWorkspaces
             }
