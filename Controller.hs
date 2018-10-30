-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

speed :: Float
speed = 1

pressed :: Bool
pressed = False

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return $ gstate { elapsedTime = elapsedTime gstate + secs }
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

{-inputKeyBool :: Event -> GameState -> Bool
inputKeyBool _ gstate =-} 
--zeroLives :: Int -> Ga

inputKeyGame :: Event -> GameState -> GameState
inputKeyGame (EventKey (Char 'w') Down _ _) gstate
 = gstate { upVector = sin(( 90 - rightVector gstate + leftVector gstate)/(180*pi)),  leftUpVector = cos((90 + leftVector gstate)/(180*pi)), rightUpVector = cos((90 + rightVector gstate)/(180*pi)) }
inputKeyGame (EventKey (Char 'a') _ _ _) gstate
 = gstate { leftVector = leftVector gstate - speed  }
inputKeyGame (EventKey (Char 'd') _ _ _) gstate
 = gstate { rightVector = rightVector gstate + speed  }
inputKeyGame (EventKey (Char 'q') Down _ _) gstate 
 = gstate { score = score gstate + 5}
inputKeyGame (EventKey (Char 'k') Down _ _) gstate
 = gstate { lives = lives gstate - 1} 
inputKeyGame _ gstate = gstate
 
inputKeyMenu :: Event -> GameState -> GameState
inputKeyMenu (EventKey (SpecialKey KeyEnter) _ _ _) gstate
 = GamePlaying ShowGame 0 0 0 0 0 3 0 0
inputKeyMenu (EventKey (Char 'h') _ _ _) gstate
 = GameHighScore ShowHighScore 0
inputKeyMenu _ gstate = gstate

inputKeyHigh :: Event -> GameState -> GameState
inputKeyHigh (EventKey (Char 'b') _ _ _) gstate
 = GameMenu ShowMenu 0 
inputKeyHigh _ gstate = gstate

inputKey :: Event -> GameState -> GameState
inputKey event gstate = case gstate of 
 GamePlaying _ _ _ _ _ _ _ _ _ -> inputKeyGame event gstate
 GameMenu _ _ -> inputKeyMenu event gstate
 GameHighScore _ _ -> inputKeyHigh event gstate 

{-inputKey (EventKey (Char 'p') _ _ _) gstate
 = undefined-}
