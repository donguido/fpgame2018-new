-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES {-&& gamePaused gstate == False-}
  = -- We show a new random number
    do randomNumber <- randomIO
       let newNumber = abs randomNumber `mod` 10
       return $ GameState (ShowANumber newNumber) 0 0
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  = gstate { infoToShow = ShowAChar c }
inputKey (EventKey (Char 'w') _ _ _) gstate
 = gstate{ upVector = upVector + 1 }
inputKey _ gstate = gstate -- Otherwise keep the same