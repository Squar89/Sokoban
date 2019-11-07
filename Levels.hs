module Levels where

import Types

mazes :: [Maze]
mazes = [level1, level2, level3, maze7]

badMazes :: [Maze]
badMazes = [level4, level5, level6, badMaze1, badMaze2, badMaze3, badMaze4]

--default level
maze1 :: MazeT
maze1 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

level1 :: Maze
level1 = Maze (C 1 (-1)) maze1

--easy correct level
maze2 :: MazeT
maze2 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == -1 && y > -3        = Wall
  | x >= 2 && y <= 0         = Wall
  | x == 0 && y == 3         = Storage
  | x == 1 && y == -3        = Storage
  | x == 1 && y == -2        = Box
  | x == -2 && y == 1        = Box
  | otherwise                = Ground
  
level2 :: Maze
level2 = Maze (C 1 (-1)) maze2

--easy correct level
maze3 :: MazeT
maze3 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 2 && abs y == 1     = Wall
  | x == 3 && abs y <= 1     = Storage
  | x == 1 && abs y <= 1     = Box
  | otherwise                = Ground
  
level3 :: Maze
level3 = Maze (C (-1) 0) maze3

--seems fine but impossible level
maze4 :: MazeT
maze4 (C x y)
  | abs x > 4  || abs y > 4        = Blank
  | abs x == 4 || abs y == 4       = Wall
  | x >= 3                          = Wall
  | x == 2 && y >= 0               = Wall
  | x == 1 && y >= 1               = Wall
  | x <= -1 && y == 1              = Wall
  | x == -3 && y == 0              = Wall
  | (x == -1 || x == 0) && y == -1 = Wall
  | x <= 0 && y == -3              = Wall
  | x == 1 && y == -1              = Storage
  | x == -2 && y == 2              = Box
  | otherwise                      = Ground

level4 :: Maze
level4 = Maze (C (-3) 2) maze4

--storages are blocked
maze5 :: MazeT
maze5 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y == 0        = Wall
  | x ==  3 && y <= -1       = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground
  
level5 :: Maze
level5 = Maze (C 1 (-1)) maze5

--not enclosed by walls
maze6 :: MazeT
maze6 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground
  
level6 :: Maze
level6 = Maze (C 1 (-1)) maze6

-----------------------------------------------maze Rafal
maze7 :: Maze
maze7 = Maze (C 0 1) maze
  where maze (C x y)
          | abs x > 3  || abs y > 2  = Blank
          | abs x == 3 || abs y == 2 = Wall
          | x == 2     || y == 1      = Storage
          | abs x == 2 || abs y == 1 = Ground
          | abs x == 1                = Wall
          | otherwise                 = Box
          
badMaze1 :: Maze
badMaze1 = Maze (C 0 (-2)) maze
  where maze (C x y)
          | abs x > 3  || abs y > 3  = Blank
          | abs x == 3 || abs y == 3 = Wall
          | abs x == 2 && y == 0     = Box
          | abs x == 2 || abs y == 2 = Ground
          | x == 0 && y >= 0          = Storage
          | otherwise                 = Wall

badMaze2 :: Maze
badMaze2 = Maze (C 0 (-2)) maze
  where maze (C x y)
          | abs x > 3  || abs y > 4  = Blank
          | x == 0     && y == 4     = Box
          | abs x == 3 || abs y == 4 = Wall
          | abs x == 2 && y <= -2    = Storage
          | abs x == 1 && y == -1    = Storage 
          | abs x <= 1 && y <= -2    = Box
          | otherwise                 = Ground

badMaze3 :: Maze
badMaze3 = Maze (C 0 0) maze
  where maze (C x y)
          | abs x > 4  || abs y > 1  = Blank
          | abs x == 4 || abs y == 1 = Wall
          | abs x == 2 &&     y == 0 = Box
          |     x == 3 &&     y == 0  = Storage 
          | otherwise                 = Ground

badMaze4 :: Maze
badMaze4 = Maze (C 0 0) maze
  where maze (C x y)
          |     x == 4 &&     y == 1      = Ground
          |     x == 4 && abs y < 4       = Wall 
          | abs x > 3  || abs y > 3       = Blank
          |     x == 3 && y < 3 && y >= 0 = Ground
          | abs x == 3  || abs y == 3     = Wall
          | abs x == 2 &&  abs y == 1 && x * y > 0  = Box
          |     x < 0 &&       y == -2    = Storage 
          | otherwise                     = Ground
---------------------------------------------------------
