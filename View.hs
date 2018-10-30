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
  ShowMenu ->  pictures[ scale (0.5) (0.5) (translate (0) (150) (color white(text "Asteroids"))) 
                         , scale (0.5) (0.5) (translate (-350) (-20) (color white (text "press enter to play"))) 
						 , scale (0.5) (0.5) (translate (-350) (-150) (color white (text "press h to see highscores")))]
  ShowGame -> pictures[rotate (0 + leftVector gstate + rightVector gstate)(translate (0) (0 + upVector gstate) (color white (polygon [(-25,0),(25,0),(0,50)])))
                              , translate (0) (50 + elapsedTime gstate) (color blue(circleSolid 2))]
  ShowHighScore -> pictures[ scale (0.5) (0.5) (translate (-350) (0) (color white(text "High Score"))) ]
	{-where  g = mkStdGen 10
	       random = randomR (1, 10) g
		   r -> fst(random)-}
