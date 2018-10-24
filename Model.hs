-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | Drawing Picture				

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
				 , upVector :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing 0 0 

type AsteroidVector = ( Float,Float )
type PlayerSpeed = Int  
type PlayerVector = ( Float,Float )
type AsteroidSpeed = Int
type Name = String
type Score = Int
type LevelName = String
type BulletVector = ( Float,Float )
type ShootingRange = ( Float,Float )

data Asteroid =  Small | Medium | Big
data Level = LvlOne | LvlTwo | LvlThree | LvlFour
--data GameStatus = GamePaused | GameOver | HighScore | GameMenu | GameFinished | GamePlaying
data Lives = ThreeLives | TwoLives | OneLives | ZeroLives
data PlayGameButton = PlayOn | PlayOff
data Collision =  IsCollided | NotCollided 
data InputCommands = Forward | Left | Right | Shoot
data OkButton = OkOn | OkOff
data PausedState = IsPaused | NotPaused
