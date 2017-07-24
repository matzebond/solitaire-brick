module UI where

import qualified Graphics.Vty as V

import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.AttrMap

import Solitaire

ui :: Widget n
ui = withBorderStyle unicode $
  borderWithLabel (str "Hello!") $
  (center (str "Left") <+> vBorder <+> center (str "Right"))

app :: App () e ()
app =
    App { appDraw = const [ui]
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return
        , appAttrMap = const $ attrMap V.defAttr []
        , appChooseCursor = neverShowCursor
        }
