-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

rotateSpeed :: Float
rotateSpeed = 15
-- This is the defition of the rotateSpeed value we use in this controller class. 

moveSpeed :: Float
moveSpeed = 10
-- this is the defition of the moveSpeed value we use in this controller class.

pressed :: Bool
pressed = False

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = case gstate of
  GameMenu _ _ _ False -> do readscore <- readFile "HighScore.txt"
                             return $ gstate { highScoreList = readscore, readHighList = True}
  GamePaused _ _ _ _ _ _ _ _ _ _ _ _-> return $ gstate { elapsedTime = elapsedTime gstate }
  GameOver _ _ _ _ False -> do 
                            let addscore = highScoreList gstate ++ " " ++ show(score gstate)
                            writescore <- writeFile ("HighScores.txt") addscore
                            copyscore <- writeFile "Highscore.txt" addscore
                            return $ gstate { saved = True } 
  GameHighScore _ _ _ False -> do highScoreL <- readFile "HighScores.txt"
                                  return $ gstate { readed = True, highScoreList = highScoreL }    
  _ -> return $ gstate { elapsedTime = elapsedTime gstate + secs }
--the step function handles the iterations of the game. it only updates the time when the game is not paused.
  {-| elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES 
  = -- We show a new random number
    do randomNumber <- randomIO
       let newNumber = abs randomNumber `mod` 10
       return $ GamePlaying (ShowANumber newNumber) 0 0 0 0
  | otherwise-}
 -- Just update the elapsed time

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)
-- the input function handles the player input in the game in each gamestate where the player is in and change the IO of the gamestate.

degreesToRad :: Float -> Float
degreesToRad x = x*(pi/180)
-- the degreesToRad function convert a degree to a radian.

inputKeyGame :: Event -> GameState -> GameState
-- the inputKeyGame function handles all the player input when he is playing the game.
inputKeyGame (EventKey (Char 'w') Down _ _) gstate
 = gstate { upVector = upVector gstate + (moveSpeed * sin(degreesToRad (90 + rightVector gstate + leftVector gstate)))
          , xNew  = xNew gstate  - (moveSpeed * cos(degreesToRad (90 + rightVector gstate + leftVector gstate)))
}
-- when the player press the w key the spaceship moves forward in the right direction.
inputKeyGame (EventKey (Char 'a') Down _ _) gstate
 = gstate { leftVector = leftVector gstate - rotateSpeed }
-- when the player press the a key the spaceship rotate around it s own axis to the left.
inputKeyGame (EventKey (Char 'd') Down _ _) gstate
 = gstate { rightVector = rightVector gstate + rotateSpeed }
-- when the player press the d key the spaceship rotate around it s own axis to the right.
inputKeyGame (EventKey (Char 'q') Down _ _) gstate 
 = gstate { score = score gstate + 5}
inputKeyGame (EventKey (Char 'k') Down _ _) gstate
 = gstate { lives = lives gstate - 1}  
inputKeyGame (EventKey (Char 'p') Down _ _) gstate
 = GamePaused ShowPause (elapsedTime gstate) (upVector gstate) (rightVector gstate) (leftVector gstate) (xNew gstate) 
                        (score gstate) (lives gstate) (bullets gstate) (bulletList gstate)(bulletX gstate)(bulletY gstate)
 -- when the player press the p key the game paused.
 
inputKeyGame (EventKey (Char 'n' ) Down _ _) gstate
  = gstate {  bullets = bullets gstate + 1 ,  bulletList = (bulletList gstate) ++ (replicate (bullets gstate) (bullet))}   
             where bullet = translate (0 + xNew gstate) (0 +upVector gstate) (rotate (0 + leftVector gstate + rightVector gstate) (color blue(circleSolid 3)))

inputKeyGame _ gstate = case lives gstate of
  0 -> GameOver ShowGameOver 0 (score gstate) (highScoreList gstate) False 
  _ -> gstate
-- we check if the player has zero lives. if the player has zero lives then it is gameover and the game goes to the gameover gamestate.
-- for every other input the game will not be affected.
 
inputKeyMenu :: Event -> GameState -> GameState
-- the inputKeyMenu function handles all the player input when the player is in the gamemenu.
inputKeyMenu (EventKey (SpecialKey KeyEnter) _ _ _) gstate
 = GamePlaying ShowGame 0 0 0 0 0 0 3 0 [] 0 0 (highScoreList gstate) 
-- when the player press the enter key, the game will start.
inputKeyMenu (EventKey (Char 'h') _ _ _) gstate
 = GameHighScore ShowHighScore 0 ""  False
-- when the player press the h key, he will go to the highscorelist.
inputKeyMenu _ gstate = gstate
-- for every other input the gamemenu will not be affected.

inputKeyHigh :: Event -> GameState -> GameState
-- the inputKeyHigh function handles all the player input when the player is in the highscorelist.
inputKeyHigh (EventKey (Char 'b') _ _ _) gstate
 = GameMenu ShowMenu 0 " " False 
-- when the player press the b key, the player will return to the gamemenu.
inputKeyHigh _ gstate = gstate
-- for every other input the highscorelist will not be affected.

inputKeyOver :: Event -> GameState -> GameState
-- the inputKeyOver function handles all the player input when the player is in the gameoverscreen.
inputKeyOver (EventKey (SpecialKey KeyEnter) _ _ _) gstate
 = GamePlaying ShowGame 0 0 0 0 0 0 3 0 [] 0 0 (highScoreList gstate)
-- when the player press the enter key, the game will start again.
inputKeyOver (EventKey (Char 'h') _ _ _) gstate
 = GameHighScore ShowHighScore 0 "" False
-- when the player press the h key, he will go to the highscorelist.
inputKeyOver _ gstate = gstate
-- for every other input the gameoverscreen will not be affected.

inputKeyPaused :: Event -> GameState -> GameState
-- the inputKeyPaused function handles all the player input when the game is paused
inputKeyPaused (EventKey (Char 'p') Down _ _) gstate
 = GamePlaying ShowGame (elapsedTime gstate) (upVector gstate) (rightVector gstate) (leftVector gstate) (xNew gstate) (score gstate) 
                        (lives gstate) (bullets gstate)(bulletList gstate)(bulletX gstate)(bulletY gstate) (highScoreList gstate)
-- when the player press the p key again, then the game will resume normally.
inputKeyPaused _ gstate = gstate
-- for every other input the pausedscreen will not be affected.

inputKey :: Event -> GameState -> GameState
inputKey event gstate = case gstate of 
 GamePlaying _ _ _ _ _ _ _ _ _ _ _ _ _ -> inputKeyGame event gstate
 GameMenu _ _ _ _ -> inputKeyMenu event gstate
 GameHighScore _ _ _ _ -> inputKeyHigh event gstate 
 GameOver _ _ _ _ _ -> inputKeyOver event gstate
 GamePaused _ _ _ _ _ _ _ _ _ _ _ _ -> inputKeyPaused event gstate
-- the inputKey function will evaluated in which gamestate the player is, and will handle only input of the player that are used for the gamestate.
