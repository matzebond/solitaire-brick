module UI where

import Lens.Micro ((^.))
import Data.Array (elems)
import Data.Maybe (listToMaybe)
import Brick
import Brick.Widgets.Border as B
import Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Brick.AttrMap

import Solitaire
import PlayingCards

type Name = ()

ui :: SolitaireGame -> [Widget Name]
ui game = [hLimit 40 $ vBox[drawPile game <+> (padLeft Max (drawFoundations game)),
                            hCenter $ drawTableau game]
          ]

drawTableau :: SolitaireGame -> Widget Name
drawTableau game = hBox columns
  where columns = map (padRight (Pad 2) . drawColumn) (elems $ game ^. tableau)

drawColumn :: Column -> Widget Name
drawColumn col = vBox (str "__" : hiddenC ++ frontC)
  where hiddenC = map (\c -> str "**") (col ^. hiddenS)
        frontC = map (\c -> str $ abbrev c) (col ^. frontS)

drawFoundations :: SolitaireGame -> Widget Name
drawFoundations game = hBox founds
  where founds = map (padRight (Pad 2) . strMaybeList "--" (abbrev)) (elems $ game ^. foundations)

drawPile :: SolitaireGame -> Widget Name
drawPile game = drawS <+> wasteP
  where drawS = strMaybeList "--" (\_ -> "**") $ game ^. drawStack
        wasteP = vBox $ map (padRight (Pad 2) . str . abbrev) (take 3 $ game ^. wastePile)

strMaybeList a f = str . maybe a f . listToMaybe

app :: App SolitaireGame e Name
app =
    App { appDraw = ui
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return
        , appAttrMap = const $ attrMap V.defAttr []
        , appChooseCursor = neverShowCursor
        }

-- handleEvent :: SolitaireGame -> BrickEvent Name Tick -> EventM Name (Next Game)
-- handleEvent g (VytEvent (V.EvKey (V.KChar 'a') [])) = Continue g
