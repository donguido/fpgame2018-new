-- | This module defines how to turn
--   the game state into a picture
module View where
import Graphics.Gloss
import Model
import System.Random

view :: GameState -> IO Picture
view = return . viewPure
-- this view function return the viewpure function to the IO.


merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

drawBullet :: Bullet -> Picture
drawBullet x = translate (bulletX x) (bulletY x) (color blue(circleSolid 3))

viewPure :: GameState -> Picture
-- the function viewPure writes for each gamestate figures and text to the screen.
viewPure gstate = case infoToShow gstate of
  ShowMenu ->  pictures[ scale (0.5) (0.5) (translate (0) (150) (color white(text "Asteroids"))) 
                         , scale (0.5) (0.5) (translate (-350) (-20) (color white (text "press enter to play"))) 
                         , scale (0.5) (0.5) (translate (-350) (-150) (color white (text "press h to see highscores")))]
-- for the gameMenu we write the titlescreen, with instructions with how to start the game and to see the highscorelist.
						 
  ShowGame -> pictures(merge [ player
                      , scale (0.2)(0.2) (translate (-1500)( 1650) (color white(text ("Score:" ++ show (score gstate)))))
                      , scale (0.2)(0.2) (translate (-1500)( 1500) (color white(text ("Lives:" ++ show (lives gstate)))))
                      , asteroid]  $ map drawBullet (bulletList gstate))
                        where player = (translate (0 + xVector gstate) (0 + yVector gstate)(rotate (0 + leftVector gstate + rightVector gstate)(color white (line [(-25, -15), (0,0),(25,-15),(0,50),(-25,-15)]))))
                              asteroid = translate (0 + asteroidxVector ) (0 - asteroidyVector) (color red(thickCircle 20 1))
                              asteroidxVector = elapsedTime gstate * 20 
                              asteroidyVector = elapsedTime gstate * 20 
                              
-- when the player plays the game, then the game will, dependent on the player input, write the spaceship, asteroids, bullets, score and lives on the screen.
                                    
  ShowHighScore -> pictures[ scale (0.5) (0.5) (translate (0) (150) (color white(text (highScoreList gstate))))
                            ,scale (0.5) (0.5) (translate (-450) (-350) (color white(text "Press b to go back to Menu")))]
-- when the player is in the highscoresstate, then we will show the highscorelist and a text that says that the player can return to the gameMenu.


  ShowPause -> pictures(merge [ player
						             , scale (0.2)(0.2) (translate (-1500)( 1650) (color white(text ("Score:" ++ show (score gstate)))))
						             , scale (0.2)(0.2) (translate (-1500)( 1500) (color white(text ("Lives:" ++ show (lives gstate)))))
                         , asteroid
                         , scale (0.5) (0.5) (translate (0) (500) (color white(text "Pause")))] $ map drawBullet (bulletList gstate))
                              where player = (translate (0 + xVector gstate) (0 + yVector gstate)(rotate (0 + leftVector gstate + rightVector gstate)(color white (line [(-25, -15), (0,0),(25,-15),(0,50),(-25,-15)]))))
                                    asteroid = translate (0 + asteroidxVector ) (0 - asteroidyVector) (color red(thickCircle 20 1))
                                    asteroidxVector = elapsedTime gstate * 20 
                                    asteroidyVector = elapsedTime gstate * 20


                                    
-- when the player pause the game. the text pause will appear and all the spaceship, asteroids and bullets will stop moving.
  
  ShowGameOver -> pictures[ scale (0.5) (0.5) (translate (0) (150) (color white(text "Game Over")))
                            ,scale (0.5) (0.5) (translate (-350) (-20) (color white(text "Press enter to play again")))
							, scale (0.5) (0.5) (translate (-350) (-150) (color white (text "press h to see highscores")))]
-- when the player has zero lives, then the game is over and the gameoverscreen will appear. There will be an gameovertitle, and instructions with how to start the game again and to see the highscorelist.