-- | This module defines how to turn
--   the game state into a picture
module View where
import Graphics.Gloss
import Model
import System.Random

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  drawing -> pictures[translate (0) (0) (color white (polygon [(-25,0),(25,0),(0,50)])), translate (0) (50 {-+ elapsedTime gstate-}) (color blue(circleSolid 2))]
    
{-viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color white (text (show n))
  ShowAChar   c -> color white (text [c])-}
