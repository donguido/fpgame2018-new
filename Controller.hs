-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

rotateSpeed :: Float
rotateSpeed = 15

moveSpeed :: Float
moveSpeed = 10


pressed :: Bool
pressed = False

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = case gstate of
  GamePaused _ _ _ _ _ _ _ _-> return $ gstate { elapsedTime = elapsedTime gstate }
  _ -> return $ gstate { elapsedTime = elapsedTime gstate + secs }
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


degreesToRad :: Float -> Float
degreesToRad x = x*(pi/180)

inputKeyGame :: Event -> GameState -> GameState
inputKeyGame (EventKey (Char 'w') Down _ _) gstate
 = gstate { upVector = upVector gstate + (moveSpeed * sin(degreesToRad (90 + rightVector gstate + leftVector gstate)))
          , xNew  = xNew gstate  - (moveSpeed * cos(degreesToRad (90 + rightVector gstate + leftVector gstate)))
}
inputKeyGame (EventKey (Char 'a') Down _ _) gstate
 = gstate { leftVector = leftVector gstate - rotateSpeed }
inputKeyGame (EventKey (Char 'd') Down _ _) gstate
 = gstate { rightVector = rightVector gstate + rotateSpeed }
inputKeyGame (EventKey (Char 'q') Down _ _) gstate 
 = gstate { score = score gstate + 5}
inputKeyGame (EventKey (Char 'k') Down _ _) gstate
 = gstate { lives = lives gstate - 1}  
inputKeyGame (EventKey (Char 'p') Down _ _) gstate
 = GamePaused ShowPause (elapsedTime gstate) (upVector gstate) (rightVector gstate) (leftVector gstate) (xNew gstate) (score gstate) (lives gstate)
inputKeyGame _ gstate = gstate
 


inputKeyMenu :: Event -> GameState -> GameState
inputKeyMenu (EventKey (SpecialKey KeyEnter) _ _ _) gstate
 = GamePlaying ShowGame 0 0 0 0 0 0 3
inputKeyMenu (EventKey (Char 'h') _ _ _) gstate
 = GameHighScore ShowHighScore 0
inputKeyMenu _ gstate = gstate

inputKeyHigh :: Event -> GameState -> GameState
inputKeyHigh (EventKey (Char 'b') _ _ _) gstate
 = GameMenu ShowMenu 0 
inputKeyHigh _ gstate = gstate

inputKeyOver :: Event -> GameState -> GameState
inputKeyOver _ gstate = gstate

inputKeyPaused :: Event -> GameState -> GameState
inputKeyPaused (EventKey (Char 'p') Down _ _) gstate
 = GamePlaying ShowGame (elapsedTime gstate) (upVector gstate) (rightVector gstate) (leftVector gstate) (xNew gstate) (score gstate) (lives gstate)
inputKeyPaused _ gstate = gstate

inputKey :: Event -> GameState -> GameState
inputKey event gstate = case gstate of 
 GamePlaying _ _ _ _ _ _ _ _-> inputKeyGame event gstate
 GameMenu _ _ -> inputKeyMenu event gstate
 GameHighScore _ _ -> inputKeyHigh event gstate 
 GameOver _ _ -> inputKeyOver event gstate
 GamePaused _ _ _ _ _ _ _ _-> inputKeyPaused event gstate

 
zeroLives :: GameState -> GameState
zeroLives gstate = case gstate of
    GamePlaying _ _ _ _ _ _ _ _-> case lives gstate of 
      0 -> GameOver ShowGameOver 0
      _ -> gstate
    _ -> gstate