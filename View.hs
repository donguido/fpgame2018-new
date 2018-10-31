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

  ShowGame -> pictures[ (translate(0  + xNew gstate)  (0 + upVector gstate) 
                                (rotate (0 + leftVector gstate + rightVector gstate)(color white (polygon [(-25,0),(25,0),(0,50)]))))
                              , translate (0) (50 + elapsedTime gstate) (color blue(circleSolid 3))
                              , scale (0.2)(0.2) (translate (-1500)( 1650) (color white(text ("Score:" ++ show (score gstate)))))
                              , scale (0.2)(0.2) (translate (-1500)( 1500) (color white(text ("Lives:" ++ show (lives gstate)))))
                              , translate (-100 + xVector ) (200 - yVector) (color red(thickCircle 20 2))] 
                              where xVector = elapsedTime gstate * 20 
                                    yVector = elapsedTime gstate * 20 
                                    
  ShowHighScore -> pictures[ scale (0.5) (0.5) (translate (0) (150) (color white(text "High Score")))
                            ,scale (0.5) (0.5) (translate (-350) (-20) (color white(text "Press b to go back to Menu")))]



  ShowPause -> pictures[ (translate (0 + xNew gstate) (0 + upVector gstate)
                            (rotate (0 + leftVector gstate + rightVector gstate)(color white (polygon [(-25,0),(25,0),(0,50)]))))
                         , translate (0) (50 + elapsedTime gstate) (color blue(circleSolid 3))
						             , scale (0.2)(0.2) (translate (-1500)( 1650) (color white(text ("Score:" ++ show (score gstate)))))
						             , scale (0.2)(0.2) (translate (-1500)( 1500) (color white(text ("Lives:" ++ show (lives gstate)))))
                         , translate (-100 + xVector ) (200 - yVector) (color red(thickCircle 20 2))
					               , scale (0.5) (0.5) (translate (0) (500) (color white(text "Pause")))] 
                        where xVector = elapsedTime gstate * 20 
                              yVector = elapsedTime gstate * 20
                            