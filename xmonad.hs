{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}
import Data.Map (fromList, union, Map)
import Data.Monoid
import Foreign.C.Types (CUInt)
import XMonad
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsStartup)
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.Circle (Circle(..))
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Prompt (defaultXPConfig)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.StackSet (sink, greedyView, shift, StackSet)
import XMonad.Util.Cursor (setDefaultCursor)

-- * Workspaces

data MyWorkspace = Pdf | Web | Emacs | Music | Images
                 | Media | Mail | Misc | Torrent | Monitors
                 deriving (Eq, Show, Enum, Bounded)

myWorkspaces :: [String]
myWorkspaces = map show ([minBound ..] :: [MyWorkspace])

workspaceKeys :: [KeySym]
workspaceKeys = [ xK_f, xK_g, xK_c, xK_r, xK_l
                , xK_d, xK_h, xK_t, xK_n, xK_s
                ]

-- * Hooks

type LayoutStack = StackSet String (Layout Window) Window ScreenId ScreenDetail
type LayoutHook = Query (Endo LayoutStack)

workspaceHook :: LayoutHook
workspaceHook = composeAll [ "MuPDF"        ~> Pdf
                           , ".dwb-wrapped" ~> Web
                           , "Emacs"        ~> Emacs
                           , "cantata"      ~> Music
                           , "feh"          ~> Images
                           , "mcomix"       ~> Images
                           , "ristretto"    ~> Images
                           , "mpv"          ~> Media
                           , "Thunderbird"  ~> Mail
                           ]
  where
    (~>) :: String -> MyWorkspace -> LayoutHook
    s ~> w = className =? s --> doShift (show w)

data FloatStyle = Default | Center deriving (Show, Eq)
type FloatingSetting = (FloatStyle, String)

newManageHook :: LayoutHook
newManageHook = manageDocks
                <+> composeAll (map floater floats)
                <+> workspaceHook
  where floats :: [FloatingSetting]
        floats = [ (Center, "feh") ]

        floater :: FloatingSetting -> ManageHook
        floater (t, i) = className =? i --> case t of
          Default -> doFloat
          Center -> doCenterFloat

myLayoutHook :: ModifiedLayout AvoidStruts
                (Choose Tall (Choose (Mirror Tall) (Choose Circle Full)))
                Window
myLayoutHook = avoidStruts $ tiled ||| Mirror tiled ||| Circle ||| Full
    where
      tiled = Tall nmaster delta ratio
      nmaster = 1
      delta = 3/100
      ratio = 1/2

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
     where fadeAmount = 0.55

-- * Keys

myKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
myKeys x =
    [ ((modCtrl,  xK_p), shellPrompt defaultXPConfig)
    , ((modShift, xK_y), kill)
    , ((modCtrl,  xK_e), sendMessage ToggleStruts)
    , ((modCtrl,  xK_b), spawn "amixer set 'Master' 10%-")
    , ((modCtrl,  xK_m), spawn "amixer set 'Master' 10%+")
    , ((modCtrl,  xK_l), sendMessage Expand)
    , ((modCtrl,  xK_h), sendMessage Shrink)
    , ((modCtrl,  xK_u), withFocused $ windows . sink)
    , ((modCtrl,  xK_equal), spawn "mpv `xsel`")
    , ((modCtrl,  xK_semicolon), spawn "slimlock")
    , ((modCtrl,  xK_asterisk), spawn "feh --scale-down `xsel`")
    ] ++ bindWs modM greedyView
      ++ bindWs modShift shift

  where
    bindWs :: a -> (String -> WindowSet -> WindowSet) -> [((a, KeySym), X())]
    bindWs k f = zip (zip (repeat k) workspaceKeys)
                     (map (withNthWorkspace f) [0..])

    modM, modCtrl, modShift :: CUInt
    modM = modMask x
    modCtrl =  modM .|. controlMask
    modShift = modM .|. shiftMask

-- Don't change the union order! It overrides default keys.
newKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
newKeys x =  fromList (myKeys x) `union` keys defaultConfig x

-- * Main

main :: IO ()
main = xmonad $ defaultConfig
  { borderWidth = 0
  , modMask = mod4Mask
  , terminal = "urxvt"
  , manageHook = newManageHook
  , logHook = myLogHook
  , keys = newKeys
  , layoutHook = myLayoutHook
  , handleEventHook = docksEventHook
  , XMonad.workspaces = myWorkspaces
  }
