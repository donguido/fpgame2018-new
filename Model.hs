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
-- this are the data infoToShow with the Show variables. This will be the info that the view class will need to write figures and text to the screen.

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 60

data Bullet = Bullet {
			   bulletX :: Float
			  ,bulletY :: Float
			  ,bulletXVector :: Float
			  ,bulletYVector :: Float
			  
}

data Asteroid = Asteroid {
			   asteroidX :: Float
			  ,asteroidY :: Float
			  ,asteroidXVector :: Float
			  ,asteroidYVector :: Float
}
data GameState = GamePlaying {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
				 , yVector :: Float
				 , leftVector :: Float
				 , rightVector :: Float
				 , xVector :: Float
				 , score :: Int
				 , lives :: Int
				 , bulletList :: [Bullet]
				 , asteroidList :: [Asteroid]
				 , highScoreList :: String		 
			     }
			   | GameMenu {
				   infoToShow :: InfoToShow
				 , elapsedTime :: Float
				 , highScoreList :: String
				 , readHighList :: Bool
				 } 
               | GameHighScore {
			       infoToShow :: InfoToShow
				 , elapsedTime :: Float
				 , highScoreList :: String
				 , readed :: Bool
			   }
			   | GamePaused {
			       infoToShow :: InfoToShow
				 , elapsedTime :: Float
				 , yVector :: Float
				 , leftVector :: Float
				 , rightVector :: Float
				 , xVector :: Float
				 , score :: Int
				 , lives :: Int
				 , bulletList :: [Bullet]
				 , asteroidList :: [Asteroid]
				 }
               | GameOver {
			       infoToShow :: InfoToShow
				 , elapsedTime :: Float
				 , score :: Int
				 , highScoreList :: String
				 , saved :: Bool
			   }
-- this are the gamestates that the game can be in with the variables that we use in each state.
   
initialState :: GameState
initialState = GameMenu ShowMenu 0 " " False
-- this is the initialstate in which the game will be when the player opens this program.

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


data Level = LvlOne | LvlTwo | LvlThree | LvlFour
data Lives = ThreeLives | TwoLives | OneLives | ZeroLives
data PlayGameButton = PlayOn | PlayOff
data Collision =  IsCollided | NotCollided 
data InputCommands = Forward | Left | Right | Shoot
data OkButton = OkOn | OkOff
data PausedState = IsPaused | NotPaused
