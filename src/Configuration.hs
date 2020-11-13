{-# LANGUAGE DisambiguateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Configuration where

import XMonad --hiding ((|||))
import qualified XMonad.Layout.Tabbed as Tabbed
import qualified XMonad.Layout.ShowWName as ShowWName

ws :: [String]
ws = ["1:一", "2:二", "3:三",
      "4:四", "5:五", "6:六",
      "7:七", "8:八", "9:九",
      "10:十", "11:十一", "12:十二",
      "13:十三","14:十四", "15:十五"]

wsExtra :: [(String, String)]
wsExtra = [("10:十",   "0"), ("11:十一", "p"), ("12:十二", "o")
          ,("13:十三", "i"), ("14:十四", "u")]

w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14 :: String
w1 = ws !! 0
w2 = ws !! 1
w3 = ws !! 2
w4 = ws !! 3
w5 = ws !! 4
w6 = ws !! 5
w7 = ws !! 6
w8 = ws !! 7
w9 = ws !! 8

wext :: Int -> String
wext = fst . (wsExtra !!)

w10 = wext 0
w11 = wext 1
w12 = wext 2
w13 = wext 3
w14 = wext 4

myWorkspaces :: [String]
myWorkspaces = ws


myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False


myClickJustFocuses :: Bool
myClickJustFocuses = True

base03, base02, base01, base00, base0, base1, base2, base3 :: String
yellow, orange, red, magenta, violet, blue, cyan, pink, green :: String

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"

blue    = "#268bd2"
cyan    = "#2aa198"
pink    = "#d96476"
green   = "#859900"

-- sizes
gap :: Int

gap = 5

topbar :: Dimension
topbar = 5

border :: Integer
border = 0

prompt :: Integer
prompt = 10

status :: Integer
status = 20

myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor     = "#000000"
myFocusedBorderColor    = active


active, activeWarn, inactive, focusColor, unfocusColor :: String
active      = blue
activeWarn  = red
inactive    = base02
focusColor  = blue
unfocusColor = base02


myFont, myBigFont, myWideFont :: String

myFont      = "-*-terminus-medium-*-*-*-*-160-*-*-*-*-*-*"
myBigFont   = "-*-terminus-medium-*-*-*-*-240-*-*-*-*-*-*"
myWideFont  = "xft:Eurostar Black Extended:"
           <> "style=Regular:pixelsize=180:hinting=true"

themeBackground :: String
themeBackground = "#3c3b37"

themeHighlight :: String
themeHighlight = "#f07746"

modm :: KeyMask
modm = mod4Mask

myTabTheme :: Tabbed.Theme
myTabTheme = def Tabbed.Theme
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = base03
    , inactiveTextColor     = base00
    }

topBarTheme :: Tabbed.Theme
topBarTheme = def Tabbed.Theme
    { fontName              = myFont
    , inactiveBorderColor   = blue
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

myShowWNameTheme :: ShowWName.SWNConfig
myShowWNameTheme = ShowWName.SWNC
    { swn_font    = myWideFont
    , swn_fade    = 0.5
    , swn_bgcolor = "#000000"
    , swn_color   = "#FFFFFF"
    }