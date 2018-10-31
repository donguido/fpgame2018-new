-- | This module contains the data types
--   which represent the state of the game
{-# LANGUAGE InstanceSigs #-}
module Model where

import Graphics.Gloss
data InfoToShow = ShowMenu
                | ShowGame
				| ShowHighScore
				| ShowPause
				| ShowGameOver
                {-| Drawing Picture
                | ShowANumber Int
                | ShowAChar   Char-}				

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 60

data GameState = GamePlaying {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
				 , upVector :: Float
				 , rightVector :: Float
				 , leftVector :: Float
				 , rightUpVector :: Float
				 , leftUpVector :: Float
				 , score :: Int
				 , lives :: Int
                 }
			   | GameMenu {
				   infoToShow :: InfoToShow
				 , elapsedTime :: Float
				 } 
               | GameHighScore {
			       infoToShow :: InfoToShow
				 , elapsedTime :: Float
			   }
			   | GamePaused {
			       infoToShow :: InfoToShow
				 , elapsedTime :: Float
				 , upVector :: Float
				 , rightVector :: Float
				 , leftVector :: Float
				 , rightUpVector :: Float
				 , leftUpVector :: Float
				 , score :: Int
				 , lives :: Int
			   }
			   | GameOver {
			       infoToShow :: InfoToShow
				 , elapsedTime :: Float
			   }
   
initialState :: GameState
initialState = GameMenu ShowMenu 0

--GameMenu Drawing

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
data Lives = ThreeLives | TwoLives | OneLives | ZeroLives
data PlayGameButton = PlayOn | PlayOff
data Collision =  IsCollided | NotCollided 
data InputCommands = Forward | Left | Right | Shoot
data OkButton = OkOn | OkOff
data PausedState = IsPaused | NotPaused
