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

  ShowGame -> pictures[ rotate (0 + leftVector gstate + rightVector gstate)(translate (0) (0 + upVector gstate) (color white (polygon [(-25,0),(25,0),(0,50)])))
                              , translate (0) (50 + elapsedTime gstate) (color blue(circleSolid 2))
							  , scale (0.2)(0.2) (translate (-1500)( 1650) ( color white(text ("score:" ++ show (score gstate)))))
							  , scale (0.2)(0.2) (translate (-1500)( 1500) ( color white(text ("lives:" ++ show (lives gstate)))))
                              , translate (50 + xVector ) (50 - yVector) (color red(thickCircle 20 50))] 

                              where xVector = elapsedTime gstate * 20
                                    yVector = elapsedTime gstate * 20
                                    number = randomRIO(fromEnum 1, fromEnum 10)
                                			  
  ShowHighScore -> pictures[ scale (0.5) (0.5) (translate (0) (150) (color white(text "High Score"))) 
                             , scale (0.5) (0.5) (translate (-350) (-20) (color white (text "press b to go back to Menu")))]
