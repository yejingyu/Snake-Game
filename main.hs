{-
Name: Jingyu Ye
This is a Snake game. You can choose to control the snake yourself or let
the hamilton AI do it for you. Your snake would grow longer when it eat an
apple. Don't hit the wall or yourself; otherwise the game would be over.

If you are using ghci, type main to begin the game!

  In start menu screen：
    [Up/Down] Choose who would be going to control the snake: user or
              hamilton AI.
    [Space]   Start the game with choosen controller
  In the game screen:
    [Up/Down/Left/Right] Change your snake head direction
    [Q]                  end the game and go to the game over screen
  In the game over screen:
    [Space] Go back to menu screen
    Score is how many apple you have eaten. The higher the better.
    Step is the distence your snake move. The lower the better.
-}

module Main where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

{-
Define data types and numbers in the game.
-}

type Position = (Int,Int)
type Snake = [Position]
type Apple = Position

data Direction = HeadUp | HeadRight | HeadDown | HeadLeft
                 deriving (Show, Eq)

data Player = User | AI
                deriving (Show, Eq)
                 
data World = Game { controller :: Player    -- Controller of the snake
                  , snake      :: Snake     -- Positions of snake body
                  , snakeSize  :: Int       -- Size of snake body
                  , appleLocat :: Apple     -- Position of the apple
                  , theScore   :: Int       -- Score
                  , totalStep  :: Int       -- Moved distence of snake
                  , inputDir   :: Direction -- User input head direction
                  , prevDir    :: Direction -- Previous direction
                  , randGen    :: StdGen    -- Random number generator
                  }
           | Menu StdGen Player             -- Ganerator controller
           | Over StdGen Int Int            -- Ganerator score step
                deriving (Show)

-- Determine how many square the game board should be.
worldSize :: (Int, Int)
worldSize = (16,16)

-- Determine the square size when draw the squares on screen
squareSize :: (Float,Float)
squareSize = (20,20)

-- the width and height of the world. Use for drawing the qictures
worldWidth :: Float
worldWidth = fst squareSize * fromIntegral (fst worldSize)
worldHeight :: Float
worldHeight = snd squareSize * fromIntegral (snd worldSize)
halfWidth :: Float
halfWidth = worldWidth / 2
halfHeight :: Float
halfHeight = worldHeight / 2

-- the maximum x & y where the snake can walk without hitting the wall
maxWalkableX :: Int
maxWalkableX = fst worldSize - 1
maxWalkableY :: Int
maxWalkableY = snd worldSize - 1

{-
Game mechanism.
-}

-- Initialize the game world
initGame :: StdGen -> Player -> World
initGame gen player = Game { controller = player
                            , snake      = startPoint
                            , snakeSize  = 3
                            , appleLocat = apple
                            , theScore   = 0
                            , totalStep  = 0
                            , inputDir   = HeadLeft
                            , prevDir    = HeadLeft
                            , randGen    = gen'
                            }
                            where (apple, gen') = genApple startPoint gen
                                  (x,y)         = worldSize
                                  startPoint    = [((x `div` 2), (y `div` 2))]

-- Initialize a menu screen  
initMenu :: StdGen -> World
initMenu gen = Menu gen User

-- Initialize a game over screen
initOver :: StdGen -> Int -> Int -> World
initOver gen score totalStep = Over gen score totalStep

-- Check the given point is in the list of point or not
exist :: Position -> [Position] -> Bool
exist x []                 = False
exist x (y:ys) | x == y    = True
               | otherwise = exist x ys

-- Generate an apple
genApple :: Snake -> StdGen -> (Apple, StdGen)
genApple snake gen = (apple, newGen)
                    where apple          = genAppleHelper num w snake (0,0)
                          num            = num' `mod` (w * h - (length snake))
                          (num', newGen) = next gen
                          (w,h)          = worldSize

-- A helper function to find the a new position that is not in snake
genAppleHelper :: Int -> Int -> Snake -> Position -> Position
genAppleHelper n w snake (x,y)| y >= w            = genAppleHelper n w snake (x+1, 0)
                              | exist (x,y) snake = genAppleHelper n w snake (x, y+1)
                              | n == 0            = (x,y)
                              | otherwise         = genAppleHelper (n-1) w snake (x, y+1)

-- Add new head to move the snake. if eat, don't cut tail; otherwise, cut tail
moveSnake :: Snake -> Direction -> Apple -> Int -> (Snake, Bool)
moveSnake snake dir apple snakeSize = (take snakeSize (newHead:snake) , isEating)
  where
    newHead  = moveHead dir (head snake)
    isEating = newHead == apple

-- Get the new position of the new head
moveHead :: Direction -> Position -> Position
moveHead HeadUp    (x,y) = (x,y+1)
moveHead HeadRight (x,y) = (x+1,y)
moveHead HeadDown  (x,y) = (x,y-1)
moveHead HeadLeft  (x,y) = (x-1,y)

-- Check the snake head hit the wall or not
hitWall:: Position -> Bool
hitWall (x,y) = x < 0  || y < 0 || x >= wx || y >= wy
                where (wx,wy) = worldSize

-- Check the snake is dead or not
isDead :: Snake -> Bool
isDead (head:body) = (exist head body) || (hitWall head)

-- Update the game state
updateState :: World -> World
updateState w@(Game { controller = player
                    , snake      = oldSnake
                    , snakeSize  = sSize
                    , appleLocat = apple
                    , theScore   = score
                    , totalStep  = step
                    , inputDir   = inDir
                    , prevDir    = pDir
                    , randGen    = gen
                    }) | isDead newSnake = initOver gen' score step
                       | ate             = w { snake      = newSnake
                                             , snakeSize  = sSize + 1
                                             , appleLocat = newApple
                                             , theScore   = score + 1
                                             , totalStep  = step + 1
                                             , randGen    = gen'
                                             , prevDir    = currDir
                                             }
                       | otherwise       = w { snake      = newSnake
                                             , totalStep  = step + 1
                                             , prevDir    = currDir
                                             }
                                             where
                                               currDir = if player == User then inDir else aiDir (head oldSnake)
                                               (newSnake, ate)  = moveSnake oldSnake currDir apple sSize
                                               (newApple, gen') = genApple newSnake gen

-- update the game data in every frame
update :: Float -> World -> World
update _ world@Game {} = updateState world
update _ world         = world

-- Get the head direction from a hamilton ai
aiDir :: Position -> Direction
aiDir (0,y) | y == maxWalkableX          = HeadRight
aiDir (1,0)                              = HeadLeft
aiDir (0,y)                              = HeadUp
aiDir (x,y) | x == maxWalkableX && odd y = HeadDown
aiDir (1,y) | even y                     = HeadDown
aiDir (x,y) | odd y                      = HeadRight
            | otherwise                  = HeadLeft

{-
Handleing user input
-}

-- Get the head direction from user while don't let turn back to its body
handleGame :: Key -> World -> World
handleGame (SpecialKey KeyUp)    w@Game{prevDir = prev} | prev /= HeadDown  = w{inputDir = HeadUp}
handleGame (SpecialKey KeyRight) w@Game{prevDir = prev} | prev /= HeadLeft  = w{inputDir = HeadRight}
handleGame (SpecialKey KeyDown)  w@Game{prevDir = prev} | prev /= HeadUp    = w{inputDir = HeadDown}
handleGame (SpecialKey KeyLeft)  w@Game{prevDir = prev} | prev /= HeadRight = w{inputDir = HeadLeft}
handleGame (Char       q)        Game{randGen= g,theScore= s,totalStep= p}  = initOver g s p 
handleGame _                     world                                      = world

-- handle user input in the menu screen
handleMenu :: Key -> World -> World
handleMenu (SpecialKey KeyUp)    (Menu randGen player) = Menu randGen (changeCtrl player)
handleMenu (SpecialKey KeyDown)  (Menu randGen player) = Menu randGen (changeCtrl player)
handleMenu (SpecialKey KeySpace) (Menu randGen player) = initGame randGen player
handleMenu _                     menu                  = menu

-- change the choice of ai or user
changeCtrl :: Player -> Player
changeCtrl User = AI
changeCtrl AI   = User

-- handle user input event
handler :: Event -> World -> World
handler (EventKey key Down _ _) menu@Menu{}                                  = handleMenu key menu
handler (EventKey key Down _ _) w@Game{}                                     = handleGame key w
handler (EventKey (SpecialKey KeySpace) Down _ _) (Over randGen _ _)         = initMenu randGen
handler _                       w                                            = w

{-
Displaying the game content to the screen and the helper functions for doing so.
-}

-- draw the game content to screen
drawWorld :: World -> Picture
drawWorld (Game {theScore = score, snake = snake, appleLocat = apple}) =
   pictures [ translate (-halfWidth) (halfHeight+3.0) $ scale 0.2  0.2  $ text ("Score: "++(show score))
            , translate (-halfWidth) (-halfHeight-23) $ scale 0.15 0.15 $ text ("[Q] End Game")
            , bg
            , drawApple apple
            , (drawSanke snake)
            ]
drawWorld (Menu _ player) =
  pictures [ bg
           , drawArrow player
           , translate (-80)  0      $ scale 0.4  0.4  $ (text "Snake")
           , translate (-40)  (-45)  $ scale 0.15 0.15 $ (text "User control")
           , translate (-40)  (-70)  $ scale 0.15 0.15 $ (text "AI control")
           , translate (-120) (-100) $ scale 0.15 0.15 $ (text "[Up/Down] choose controller")
           , translate (-120) (-130) $ scale 0.15 0.15 $ (text "[Space]   start")
           ]
drawWorld (Over _ score step) =
  pictures [ bg
           , translate (-120) 0      $ scale 0.3  0.3  $ (text "You Defeated")
           , translate (-120) (-45)  $ scale 0.15 0.15 $ (text ("Score: " ++ (show score)))
           , translate (-120) (-70)  $ scale 0.15 0.15 $ (text ("Step : " ++ (show step)))
           , translate (-120) (-100) $ scale 0.15 0.15 $ (text "[Space] Return to menu")
           ]

-- return the picture of snake
drawSanke :: [Position] -> Picture
drawSanke snake = translate (-halfWidth) (-halfHeight) $ color green $ pictures(map square snake)

-- return the picture of apple
drawApple :: Position -> Picture
drawApple apple = translate (-halfWidth) (-halfHeight) $ color red $ square apple

-- return the picture of arrow in menu with correct location base on the choise of player
drawArrow :: Player -> Picture
drawArrow User = translate (-80) (-45) $ scale 0.15 0.15 $ (text "->")
drawArrow AI   = translate (-80) (-70) $ scale 0.15 0.15 $ (text "->")

--draw the wall
bg :: Picture
bg = translate (-halfWidth) (-halfHeight) $
     pictures [ translate 0          halfHeight $ rectangleSolid 2.0        worldHeight
              , translate worldWidth halfHeight $ rectangleSolid 2.0        worldHeight
              , translate halfWidth  0          $ rectangleSolid worldWidth 2.0 
              , translate halfWidth  worldWidth $ rectangleSolid worldWidth 2.0
              ]

-- draw point
square :: Position -> Picture
square (x,y) = translate (w*x'+w/2) (h*y'+h/2) $ rectangleSolid w h
  where
    x'    = fromIntegral x
    y'    = fromIntegral y
    (w,h) = squareSize

{-
main function
-}
    
main :: IO ()
main = do
         randGen <- getStdGen
         play display white 5 (initMenu randGen) drawWorld handler update
           where
             display = InWindow "Snake" (round worldWidth + 20, round worldHeight + 60) (0, 0)