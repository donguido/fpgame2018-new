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
-- this function changes the x and the y position of the bullet dependent of the xvector and the yvector of the bullet.

changeBullet :: Bullet -> Bullet
changeBullet Bullet {bulletX = x , bulletY = y, bulletXVector = vx, bulletYVector = vy} = Bullet { bulletX = x + vx , bulletY = y + vy,bulletXVector = vx, bulletYVector = vy }
-- this function changes the x and the y position of the asteroid dependent of the xvector and the yvector of the asteroid.

oobBullet :: Bullet -> Bool
oobBullet Bullet {bulletX = x , bulletY = y} = ( x < -700) || ( y > 500 ) || ( x > 700 ) || ( y < -500 ) 
-- this function checks if a bullet is out of bound.

oobAsteroid :: Asteroid -> Asteroid
oobAsteroid i@Asteroid{asteroidX = x , asteroidY = y, asteroidXVector = vx, asteroidYVector = vy} 
  | x < -700 = Asteroid { asteroidX = 700, asteroidY = y , asteroidXVector = vx, asteroidYVector = vy }
  | y > 500 =  Asteroid { asteroidY = -500, asteroidX = x , asteroidXVector = vx, asteroidYVector = vy }
  | x > 700 =  Asteroid { asteroidX = -700, asteroidY = y , asteroidXVector = vx, asteroidYVector = vy }
  | y < -500 = Asteroid { asteroidY = 500, asteroidX = x, asteroidXVector = vx, asteroidYVector = vy }
  | otherwise = id i
{- this function checks if a asteroid is out of bound, if it is then the asteroid will be drawed at the other side of the screen opposite from where it left the screen.
if the asteroid is not out of bound his position will not change.-}

bulletAsteroidCollison :: [Bullet] -> [Asteroid] -> Bool
bulletAsteroidCollison [] _ = False
bulletAsteroidCollison _ [] = False
bulletAsteroidCollison (bullet@Bullet { bulletX = x1, bulletY = y1, bulletXVector = vx1, bulletYVector =vy1 } :bullets) (asteroid@Asteroid { asteroidX = x2, asteroidY = y2, asteroidXVector = vx2, asteroidYVector = vy2}:asteroids) 
  | (sqrt(abs(x1-x2)*(abs(x1-x2))+(abs(y1-y2)*abs(y1-y2))) < 40)  = True 
  | otherwise = bulletAsteroidCollison bullets asteroids  
-- this function checks if a bullet is in collison with a asteroid.
  
playerAsteroidCollison :: Float -> Float -> [Asteroid] -> Bool
playerAsteroidCollison _ _ []  = False
playerAsteroidCollison x1 y1 (asteroid@Asteroid { asteroidX = x2, asteroidY = y2} :asteroids)
 | (sqrt(abs(x1-x2)*(abs(x1-x2))+(abs(y1-y2)*abs(y1-y2))) < 88) = True
 | otherwise = playerAsteroidCollison x1 y1 asteroids
-- this function checks if the player is in collison with a asteroid.   
  
outOfBoundY :: Float -> Float
outOfBoundY x 
  | x < -500 = 500
  | x > 500 = -500
  | otherwise = x
-- this function will gives the x position of a player is out of bound. if it is, then it will give the opposite x value back. Else the x position will not change.
 
outOfBoundX :: Float -> Float
outOfBoundX x
  | x < -500 = 500
  | x > 500 = -500
  | otherwise = x
-- this function will gives the y position of a player is out of bound. if it is, then it will give the opposite y value back. Else the y position will not change.
  
-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = case gstate of
  GameMenu _ _ _ False -> do readscore <- readFile "HighScore.txt"
                             return $ gstate { highScoreList = readscore, readHighList = True}
-- in the gamemenu state the game will read the highscore textfile and give it to the state and set the readhighlist boolean to True.
  GamePaused _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _-> return $ gstate { elapsedTime = elapsedTime gstate } 
-- if the game is paused the time will stop.
  GamePlaying _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ -> case lives gstate of 
                                                   0 -> return (GameOver ShowGameOver 0 (score gstate) (highScoreList gstate) False)
                                                   _ -> return $ gstate {elapsedTime = elapsedTime gstate + secs
-- if the player has 0 lives left in the game, then the game is over and he will go to the gameover state, else game will be updated.
                                                     ,bulletList =  filter (\x -> not (bulletAsteroidCollison ([x]) (asteroidList gstate)))(map changeBullet (bulletList gstate))													  
-- the list of bullet will be updated dependent whether a bullet is out of bound, then the bullet will be deleted. and whetehr a bullet is shooted by the player.												   
                                                     ,asteroidList = filter (\x -> not (bulletAsteroidCollison (bulletList gstate) ([x])))(map (oobAsteroid)(map changeAsteroid (asteroidList gstate)))
-- the list of asteroid will be updated dependent whether the asteroid is out of bound or not.												   
												   ,score = if bulletAsteroidCollison (bulletList gstate) (asteroidList gstate) then score gstate + 25 else score gstate
-- the score will be updated when a bullet hits a asteroid, then the score will add 25 points. Else the score will be the same.												   
												   ,lives = if isInvincible gstate then lives gstate else (if playerAsteroidCollison (xVector gstate) (yVector gstate) (asteroidList gstate) then lives gstate -1 else lives gstate)												                 
-- the lives will be updated when the player hits a asteroid, then the player will lose one live. Else the lives will be the same. no lives will be lost when the player is invincible.
                                                   ,rotatespeed = case aPressed gstate || dPressed gstate of
                                                                  True -> 10
                                                                  _ -> 0
-- the rotatespeed will be 10 when the player press the a or the d key. Else it will be zero.
                                                   ,leftVector =  case aPressed gstate of
                                                                  True -> leftVector gstate - (rotatespeed gstate)
                                                                  _ -> leftVector gstate
-- the player will rotate to the left when the a key is pressed.
                                                   ,rightVector =  case dPressed gstate of
                                                                   True -> rightVector gstate + (rotatespeed gstate)
                                                                   _ ->  rightVector gstate
-- the player will rotate to the right when the d key is pressed.                                                                
                                                   ,movespeed =  case wPressed gstate of
                                                                 True ->  10
                                                                 _ -> 0
-- the movespeed will be 10 when the player press the w key. Else it will be zero.
                                                   ,isInvincibleTime = if playerAsteroidCollison (xVector gstate) (yVector gstate) (asteroidList gstate) then 3 else isInvincibleTime gstate - secs
-- the time that the player is invincible will be reset when the player collides with a asteroid. else the invincibletime variable will subtract as the seconds pass.												   
                                                   ,isInvincible = if isInvincible gstate then (if isInvincibleTime gstate <= 0 then False else True) else (if playerAsteroidCollison (xVector gstate) (yVector gstate) (asteroidList gstate) then True else False)												                   
-- if the player is invincible then no collison with a asteroid is possible.	
                                                   , isNotBlinking = if isInvincible gstate then (if (even(round(isInvincibleTime gstate))) then True else False) else False
-- the player blink at the even seconds, when the player is invincible.												   
                                                  ,yVector = if isInvincible gstate then outOfBoundY (yVector gstate + (movespeed gstate * sinRotate)) else (if playerAsteroidCollison (xVector gstate) (yVector gstate) (asteroidList gstate) then 0 else outOfBoundY (yVector gstate + (movespeed gstate * sinRotate))) 												                  
-- the yvector will be zero when the player collides with a asteroid. Else the player y postion will be updated and checked if it is out of bound. If the player is invincible there is no collison possible	  
                                                  ,xVector = if isInvincible gstate then outOfBoundX (xVector gstate  - (movespeed gstate * cosRotate)) else (if playerAsteroidCollison (xVector gstate) (yVector gstate) (asteroidList gstate) then 0 else outOfBoundX (xVector gstate  - (movespeed gstate * cosRotate)))	}											          
                                                      where sinRotate = sin(degreesToRad (90 + rightVector gstate + leftVector gstate ))
                                                            cosRotate = cos(degreesToRad (90 + rightVector gstate  + leftVector gstate )) 
-- the xvector will be zero when the player collides with a asteroid. Else the player x postion will be updated and checked if it is out of bound. If the player is invincible there is no collison possible                                                                                                                                                         
  GameOver _ _ _ _ False -> do 
                            let addscore = highScoreList gstate ++ " " ++ show(score gstate) ++ "\n"
                            writescore <- writeFile ("HighScores.txt") addscore
                            copyscore <- writeFile "Highscore.txt" addscore
                            return $ gstate { saved = True }
-- when the game is over the score that the player has reached, will be writen to a text file. 
  GameHighScore _ _ _ False -> do highScoreL <- readFile "HighScores.txt"
                                  return $ gstate { readed = True, highScoreString = highScoreL } 
-- when the player is in the highscore gamestate, the highscore will be read from the textfile.
  _ -> return $ gstate { elapsedTime = elapsedTime gstate + secs }
-- in every other sitiation the gamestate will be updated normally.
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
-- this function creates a bullet dependent on the position of the player.

createAsteroid :: Float -> Float -> Float -> Float -> Asteroid
createAsteroid px py vx vy =  Asteroid { asteroidX = px , asteroidY = py ,  asteroidXVector = vx, asteroidYVector = vy }
-- this function creates a asteroid.

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
 = gstate { lives = lives gstate - 1 , isInvincible = True}  
inputKeyGame (EventKey (Char 'p') Down _ _) gstate
 = GamePaused ShowPause (elapsedTime gstate) (yVector gstate) (rightVector gstate) (leftVector gstate) (xVector gstate) (movespeed gstate) (rotatespeed gstate)
                        (score gstate) (lives gstate) (bulletList gstate) (asteroidList gstate) (isInvincible gstate) (isInvincibleTime gstate) (wPressed gstate) (aPressed gstate) (dPressed gstate)
 -- when the player press the p key the game paused.
 
inputKeyGame (EventKey (SpecialKey KeySpace ) Down _ _) gstate
  = gstate { bulletList = (bulletList gstate) ++ [createBullet (xVector gstate - (50*cosRotate)) (yVector gstate + (50*sinRotate))  (-15* cosRotate) ( 15*sinRotate)]}
              where sinRotate = sin(degreesToRad (90 + rightVector gstate + leftVector gstate ))
                    cosRotate = cos(degreesToRad (90 + rightVector gstate  + leftVector gstate ))
-- when the player press space, the player will shoot bullets.             
			 
inputKeyGame _ gstate = case lives gstate of
  0 -> GameOver ShowGameOver 0 (score gstate) (highScoreList gstate) False 
  _ -> gstate
-- we check if the player has zero lives. if the player has zero lives then it is gameover and the game goes to the gameover gamestate.
-- for every other input the game will not be affected.
 
inputKeyMenu :: Event -> GameState -> GameState
-- the inputKeyMenu function handles all the player input when the player is in the gamemenu.
inputKeyMenu (EventKey (SpecialKey KeyEnter) _ _ _) gstate
 = GamePlaying ShowGame 0 0 0 0 0 0 0 0 3 [] [randomAsteroid 4 52 15 3,randomAsteroid 53 32 56 2, randomAsteroid 2 42 5 23](highScoreList gstate) False 3 True False False False
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
 = GamePlaying ShowGame 0 0 0 0 0 0 0 0 3 [] [randomAsteroid 4 52 15 3,randomAsteroid 53 32 56 2, randomAsteroid 2 42 5 23](highScoreList gstate) False 3 True False False False
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
                        (lives gstate)(bulletList gstate)(asteroidList gstate)(highScoreList gstate) (isInvincible gstate) (isInvincibleTime gstate) (isNotBlinking gstate) (wPressed gstate) (aPressed gstate) (dPressed gstate)
-- when the player press the p key again, then the game will resume normally.
inputKeyPaused _ gstate = gstate
-- for every other input the pausedscreen will not be affected.

inputKey :: Event -> GameState -> GameState
inputKey event gstate = case gstate of 
 GamePlaying _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _-> inputKeyGame event gstate
 GameMenu _ _ _ _-> inputKeyMenu event gstate
 GameHighScore _ _ _ _-> inputKeyHigh event gstate 
 GameOver _ _ _ _ _-> inputKeyOver event gstate
 GamePaused _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _-> inputKeyPaused event gstate
-- the inputKey function will evaluated in which gamestate the player is, and will handle only input of the player that are used for the gamestate.