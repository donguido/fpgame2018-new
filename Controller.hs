-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

speed :: Float
speed = 1

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

inputKey :: Event -> GameState -> GameState
{-inputKey (EventKey (Char c) _ _ _) gstate
  = gstate { infoToShow = ShowAChar c }-}
inputKey (EventKey (Char 'w') _ _ _) gstate
 = gstate { upVector = upVector gstate + speed * elapsedTime gstate }
inputKey (EventKey (Char 'a') _ _ _) gstate
 = gstate { leftVector = leftVector gstate - speed * elapsedTime gstate }
inputKey (EventKey (Char 'd') _ _ _) gstate
 = gstate { rightVector = rightVector gstate + speed * elapsedTime gstate }
inputKey (EventKey (Char 'p') _ _ _) gstate
 = undefined
inputKey (EventKey (Char 'h') _ _ _) gstate
 = GameHighScore ShowHighScore 0 
inputKey (EventKey (SpecialKey KeyEnter) _ _ _) gstate
 = GamePlaying ShowGame 0 0 0 0 
inputKey _ gstate = gstate -- Otherwise keep the same