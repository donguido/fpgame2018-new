-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random


-- This is the defition of the rotateSpeed value we use in this controller class. 

-- this is the defition of the moveSpeed value we use in this controller class.

changeAsteroid :: Asteroid -> Asteroid
changeAsteroid Asteroid {asteroidX = x , asteroidY = y, asteroidXVector = vx, asteroidYVector = vy} = Asteroid { asteroidX = x + vx , asteroidY = y + vy, asteroidXVector = vx, asteroidYVector = vy }


changeBullet :: Bullet -> Bullet
changeBullet Bullet {bulletX = x , bulletY = y, bulletXVector = vx, bulletYVector = vy} = Bullet { bulletX = x + vx , bulletY = y + vy,bulletXVector = vx, bulletYVector = vy }


oobBullet :: Bullet -> Bool
oobBullet Bullet {bulletX = x , bulletY = y} = ( x < -700) || ( y > 500 ) || ( x > 700 ) || ( y < -500 ) 

oobAsteroid :: Asteroid -> Asteroid
oobAsteroid i@Asteroid{asteroidX = x , asteroidY = y, asteroidXVector = vx, asteroidYVector = vy} 
  | x < -700 = Asteroid { asteroidX = 700, asteroidY = y , asteroidXVector = vx, asteroidYVector = vy }
  | y > 500 =  Asteroid { asteroidY = -500, asteroidX = x , asteroidXVector = vx, asteroidYVector = vy }
  | x > 700 =  Asteroid { asteroidX = -700, asteroidY = y , asteroidXVector = vx, asteroidYVector = vy }
  | y < -500 = Asteroid { asteroidY = 500, asteroidX = x, asteroidXVector = vx, asteroidYVector = vy }
  | otherwise = id i

outOfBoundY :: Float -> Float
outOfBoundY x 
  | x < -500 = 500
  | x > 500 = -500
  | otherwise = x
 
outOfBoundX :: Float -> Float
outOfBoundX x
  | x < -500 = 500
  | x > 500 = -500
  | otherwise = x


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = case gstate of
  GameMenu _ _ _ False -> do readscore <- readFile "HighScore.txt"
                             return $ gstate { highScoreList = readscore, readHighList = True}
  GamePaused _ _ _ _ _ _ _ _ _ _ _ _ _ _ _-> return $ gstate { elapsedTime = elapsedTime gstate } 
  GamePlaying _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _-> return $ gstate {elapsedTime = elapsedTime gstate + secs 
                                                   ,bulletList =  filter (\x -> not (oobBullet x) )(map changeBullet (bulletList gstate))
                                                   ,asteroidList = map (oobAsteroid)(map changeAsteroid (asteroidList gstate))
                                                   
                                                   ,rotatespeed = case aPressed gstate || dPressed gstate of
                                                                  True -> 10
                                                                  _ -> 0

                                                   ,leftVector =  case aPressed gstate of
                                                                  True  -> leftVector gstate - (rotatespeed gstate)
                                                                  _  -> leftVector gstate

                                                   ,rightVector =  case dPressed gstate of
                                                                   True -> rightVector gstate + (rotatespeed gstate)
                                                                   _ ->  rightVector gstate
                                                   
                                                   
                                                   ,movespeed =  case wPressed gstate of
                                                                 True ->  10
                                                                 _ -> 0

                                                  , yVector = outOfBoundY (yVector gstate + (movespeed gstate * sinRotate))
                                                  , xVector = outOfBoundX (xVector gstate  - (movespeed gstate * cosRotate))}
                                                      where sinRotate = sin(degreesToRad (90 + rightVector gstate + leftVector gstate ))
                                                            cosRotate = cos(degreesToRad (90 + rightVector gstate  + leftVector gstate )) 
                                                                                                       
                                                   
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

createBullet :: Float -> Float -> Float -> Float -> Bullet
createBullet px py vx vy =  Bullet { bulletX = px , bulletY = py ,  bulletXVector = vx, bulletYVector = vy }


createAsteroid :: Float -> Float -> Float -> Float -> Asteroid
createAsteroid px py vx vy =  Asteroid { asteroidX = px , asteroidY = py ,  asteroidXVector = vx, asteroidYVector = vy }



randomAsteroid :: Int -> Int -> Int -> Int -> Asteroid
randomAsteroid rx ry rvx rvy= createAsteroid (fst (randomR (-600,600) (mkStdGen rx))) (fst (randomR (-400,400) (mkStdGen ry))) (fst (randomR (-10,10) (mkStdGen rvx))) (fst (randomR (-10,10) (mkStdGen rvy))) 

{-(fst(randomR (-100, 200) (mkStdGen rx))) (fst(randomR (-100, 200) (mkStdGen ry))) (fst(randomR (-5 , 5) (mkStdGen rvx)))  (fst(randomR (-5 , 5) (mkStdGen rvy))) 
-}
degreesToRad :: Float -> Float
degreesToRad x = x*(pi/180)
-- the degreesToRad function convert a degree to a radian.




inputKeyGame :: Event -> GameState -> GameState
-- the inputKeyGame function handles all the player input when he is playing the game.
inputKeyGame (EventKey (Char 'w') Down _ _) gstate
 = gstate { wPressed = True } 

inputKeyGame (EventKey (Char 'w') Up _ _) gstate
 = gstate { wPressed = False }
 



-- when the player press the w key the spaceship moves forward in the right direction.
inputKeyGame (EventKey (Char 'a') Down _ _) gstate
 = gstate { aPressed = True }

inputKeyGame (EventKey (Char 'a') Up _ _) gstate
 = gstate { aPressed = False }

-- when the player press the a key the spaceship rotate around it s own axis to the left.
inputKeyGame (EventKey (Char 'd') Down _ _) gstate
 = gstate { dPressed = True }

inputKeyGame (EventKey (Char 'd') Up _ _) gstate
 = gstate { dPressed = False }
-- when the player press the d key the spaceship rotate around it s own axis to the right.
inputKeyGame (EventKey (Char 'q') Down _ _) gstate 
 = gstate { score = score gstate + (fst (randomR (0, 10) (mkStdGen 49)))}
inputKeyGame (EventKey (Char 'k') Down _ _) gstate
 = gstate { lives = lives gstate - 1}  
inputKeyGame (EventKey (Char 'p') Down _ _) gstate
 = GamePaused ShowPause (elapsedTime gstate) (yVector gstate) (rightVector gstate) (leftVector gstate) (xVector gstate) (movespeed gstate) (rotatespeed gstate)
                        (score gstate) (lives gstate) (bulletList gstate) (asteroidList gstate)(wPressed gstate) (aPressed gstate) (dPressed gstate)
 -- when the player press the p key the game paused.
 
inputKeyGame (EventKey (SpecialKey KeySpace ) Down _ _) gstate
  = gstate { bulletList = (bulletList gstate) ++ [createBullet (xVector gstate - (50*cosRotate)) (yVector gstate + (50*sinRotate))  (-15* cosRotate) ( 15*sinRotate)]}
              where sinRotate = sin(degreesToRad (90 + rightVector gstate + leftVector gstate ))
                    cosRotate = cos(degreesToRad (90 + rightVector gstate  + leftVector gstate ))
             



inputKeyGame _ gstate = case lives gstate of
  0 -> GameOver ShowGameOver 0 (score gstate) (highScoreList gstate) False 
  _ -> gstate
-- we check if the player has zero lives. if the player has zero lives then it is gameover and the game goes to the gameover gamestate.
-- for every other input the game will not be affected.
 
inputKeyMenu :: Event -> GameState -> GameState
-- the inputKeyMenu function handles all the player input when the player is in the gamemenu.
inputKeyMenu (EventKey (SpecialKey KeyEnter) _ _ _) gstate
 = GamePlaying ShowGame 0 0 0 0 0 0 0 0 3 [] [randomAsteroid 4 52 15 3,randomAsteroid 53 32 56 2, randomAsteroid 2 42 5 23](highScoreList gstate) False False False
-- when the player press the enter key, the game will start.
inputKeyMenu (EventKey (Char 'h') _ _ _) gstate
 = GameHighScore ShowHighScore 0 "" False
-- when the player press the h key, he will go to the highscorelist.
inputKeyMenu _ gstate = gstate
-- for every other input the gamemenu will not be affected.

inputKeyHigh :: Event -> GameState -> GameState
-- the inputKeyHigh function handles all the player input when the player is in the highscorelist.
inputKeyHigh (EventKey (Char 'b') _ _ _) gstate
 = GameMenu ShowMenu 0 "" False
-- when the player press the b key, the player will return to the gamemenu.
inputKeyHigh _ gstate = gstate
-- for every other input the highscorelist will not be affected.

inputKeyOver :: Event -> GameState -> GameState
-- the inputKeyOver function handles all the player input when the player is in the gameoverscreen.
inputKeyOver (EventKey (SpecialKey KeyEnter) _ _ _) gstate
 = GamePlaying ShowGame 0 0 0 0 0 0 0 0 3 [] [randomAsteroid 4 52 15 3,randomAsteroid 53 32 56 2, randomAsteroid 2 42 5 23](highScoreList gstate) False False False
-- when the player press the enter key, the game will start again.
inputKeyOver (EventKey (Char 'h') _ _ _) gstate
 = GameHighScore ShowHighScore 0  "" False
-- when the player press the h key, he will go to the highscorelist.
--inputKeyOver (EventKey (Char 's') Down _ _) gstate = gstate { newscore = writeFile "C:/Users/Guido/OneDrive/Documenten/fp asteroid game 2018/highscores.txt" (show(score gstate)) }
inputKeyOver _ gstate = gstate
-- for every other input the gameoverscreen will not be affected.

inputKeyPaused :: Event -> GameState -> GameState
-- the inputKeyPaused function handles all the player input when the game is paused
inputKeyPaused (EventKey (Char 'p') Down _ _) gstate
 = GamePlaying ShowGame (elapsedTime gstate) (yVector gstate) (rightVector gstate) (leftVector gstate) (xVector gstate) (movespeed gstate)(rotatespeed gstate) (score gstate) 
                        (lives gstate)(bulletList gstate)(asteroidList gstate)(highScoreList gstate) (wPressed gstate) (aPressed gstate) (dPressed gstate)
-- when the player press the p key again, then the game will resume normally.
inputKeyPaused _ gstate = gstate
-- for every other input the pausedscreen will not be affected.

inputKey :: Event -> GameState -> GameState
inputKey event gstate = case gstate of 
 GamePlaying _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _-> inputKeyGame event gstate
 GameMenu _ _ _ _-> inputKeyMenu event gstate
 GameHighScore _ _ _ _-> inputKeyHigh event gstate 
 GameOver _ _ _ _ _-> inputKeyOver event gstate
 GamePaused _ _ _ _ _ _ _ _ _ _ _ _ _ _ _-> inputKeyPaused event gstate
-- the inputKey function will evaluated in which gamestate the player is, and will handle only input of the player that are used for the gamestate.