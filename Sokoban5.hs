{-# LANGUAGE OverloadedStrings #-}
import Data.Text
import System.IO
import Types
import Levels

main :: Program
main = etap5

write :: String -> IO ()
write = putStr

writeln :: String -> IO ()
writeln = putStrLn

wall :: Picture
wall drawFun = drawFun'
  where drawFun' 0 0 = '#'
        drawFun' x y = drawFun x y

ground :: Picture
ground drawFun = drawFun'
  where drawFun' 0 0 = ' '
        drawFun' x y = drawFun x y

storage :: Picture
storage drawFun = drawFun'
  where drawFun' 0 0 = '.'
        drawFun' x y = drawFun x y

box :: Picture
box drawFun = drawFun'
  where drawFun' 0 0 = '$'
        drawFun' x y = drawFun x y
    
blank :: Picture
blank = id

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

player :: Picture
player drawFun = drawFun'
  where drawFun' 0 0 = '@'
        drawFun' x y = drawFun x y

translated :: Integer -> Integer -> Picture -> Picture
translated x y p = (\g -> (\x' y' -> p (\x'' y'' -> g (x'' + x) (y'' - y)) (x' - x) (y' + y)))

pictureOfMazeHelp :: MazeT -> [Coord] -> Picture
pictureOfMazeHelp _ [] = blank
pictureOfMazeHelp maze ((C x y):t) = (translated x y (drawTile (maze (C x y))))
                                     & (pictureOfMazeHelp maze t)

pictureOfMaze :: MazeT -> Picture
pictureOfMaze maze = pictureOfMazeHelp maze (getAllCoords maze)

getAllCoords :: MazeT -> [Coord]
getAllCoords mazeF = findReachable neighbours [] [C 0 0]
  where neighbours c
          | (mazeF c) == Blank = []
          | otherwise = Prelude.filter (\c' -> elem (mazeF c') [Wall, Storage, Box, Ground, Blank]) (getNeighbourCoord c)

removeBoxes :: MazeT -> MazeT
removeBoxes maze' = f . maze' where f = (\tile -> if tile == Box then Ground else tile)

addBoxes :: [Coord] -> MazeT -> MazeT
addBoxes l maze' = modMaze where modMaze c = if elem c l then Box else maze' c

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated x y pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C x (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C x (y-1)

isFreeTile :: Tile -> Bool
isFreeTile tile  = elem tile [Ground, Storage]

checkMove :: Coord -> State -> State
checkMove oldC state@(S c moveD l ((Maze initC maze):tM) cnt)
  | elem c l = moveBox oldC state
  | isFreeTile (removeBoxes maze c) = (S c moveD l ((Maze initC maze):tM) (cnt+1))
checkMove oldC (S _ moveD l ((Maze initC maze):tM) cnt) = (S oldC moveD l ((Maze initC maze):tM) cnt)
checkMove _ (S _ _ _ [] _) = error "No level present"

moveBox :: Coord -> State -> State
moveBox _ (S c moveD l ((Maze initC maze):tM) cnt)
  | isFreeTile (addBoxes l (removeBoxes maze) (adjacentCoord moveD c)) =
    (S c moveD (Prelude.map (\lc -> if lc == c then (adjacentCoord moveD c) else lc) l) ((Maze initC maze):tM) (cnt+1))
moveBox oldC (S _ moveD l ((Maze initC maze):tM) cnt) = (S oldC moveD l ((Maze initC maze):tM) cnt)
moveBox _ (S _ _ _ [] _) = error "No level present"

initialCoord :: [Maze] -> State
initialCoord mazeList@((Maze c mazeF):_) = (S c U
  (Prelude.filter (\c' -> mazeF c' == Box) (findReachable (neighboursCoordF mazeF) [] [c])) mazeList 0)
initialCoord [] = error "No level present"

handleEvent :: Event -> State -> State
handleEvent _ state@(S _ _ _ (_:[]) _)
  | isWinning state = state
handleEvent (KeyPress key) state@(S c _ l (m:tM) cnt)
  | isWinning state && key == "n" = (initialCoord tM)
  | isWinning state = state
  | key == "d"  = go R
  | key == "w"  = go U
  | key == "a"  = go L
  | key == "s"  = go D
    where go d' = (checkMove c (S (adjacentCoord d' c) d' l (m:tM) cnt))
handleEvent _ w      = w

getStorageList :: MazeT -> [Coord]
getStorageList maze' =
  (Prelude.filter (\c -> maze' c == Storage) [(C x y) | x<-[-10..10], y<-[-10..10]])

isWinning :: State -> Bool
isWinning (S _ _ l ((Maze _ maze):_) _) = and boxOnStorage
  where boxOnStorage = Prelude.map (\box' -> elem box' (getStorageList maze)) l
isWinning (S _ _ _ [] _) = error "No level present"

draw :: State -> Screen
draw state@(S _ _ _ (_:[]) cnt)
  | isWinning state = "Poziom ukończony, liczba ruchów: " ++ (show cnt) ++ "\nGratulacje, ukończyłeś wszystkie poziomy"
draw state@(S c d l ((Maze _ maze):_) cnt)
  | isWinning state = "Poziom ukończony, liczba ruchów: " ++ (show cnt) ++ "\nNacisnij n aby przejść do następnego poziomu"
  | otherwise = [if y == 11 then '\n' else (drawFun y x) | x<-[-10..10], y<-[-10..11]]
      where picture = (atCoord c player) & (pictureOfMaze (addBoxes l (removeBoxes maze)))
            drawFun = picture (\x y -> ' ')
draw (S _ _ _ [] _) = error "No level present"

getNeighbourCoord :: Coord -> [Coord]
getNeighbourCoord (C x y) = [(C (x-1) y), (C (x+1) y), (C x (y-1)), (C x (y+1))]

findReachable :: Eq a => (a -> [a]) -> [a] -> [a] -> [a]
findReachable _ reachableList [] = reachableList
findReachable neighboursF reachableList (nH:nT)
  | elem nH reachableList = findReachable neighboursF reachableList nT
  | otherwise = findReachable neighboursF (reachableList ++ [nH]) (nT ++ (neighboursF nH))

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = Prelude.all isOk reachableList
  where reachableList = findReachable neighbours [] [initial]
  
reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = elem v (findReachable neighbours [] [initial])

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = and (Prelude.map (\v -> reachable v initial neighbours) vs)

neighboursCoordF :: MazeT -> (Coord -> [Coord])
neighboursCoordF mazeF = neighbours
  where neighbours c
          | (mazeF c) == Blank = []
          | otherwise = Prelude.filter (\c' -> elem (mazeF c') [Storage, Box, Ground, Blank]) (getNeighbourCoord c)

isClosed :: Maze -> Bool
isClosed (Maze c mazeF) =
  elem (mazeF c) [Storage, Ground]
  && isGraphClosed c (neighboursCoordF mazeF) (\c' -> (mazeF c') /= Blank)
            
isSane :: Maze -> Bool
isSane (Maze c mazeF) =
  (Prelude.length (Prelude.filter (\c' -> (mazeF c') == Storage) reachableList))
  >= (Prelude.length (Prelude.filter (\c' -> (mazeF c') == Box) reachableList))
    where reachableList = findReachable (neighboursCoordF mazeF) [] [c]

resettable :: Interaction s -> Interaction s
resettable (Interaction state0 handle draw')
  = Interaction state0 handle' draw'
  where handle' (KeyPress key) _ | key == "\ESC" = state0
        handle' e s = handle e s
        
withStartScreen :: Interaction s -> Interaction (SSState s)
withStartScreen (Interaction state0 handle drawI)
  = Interaction state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = drawI s
    
withUndo :: Eq a => Interaction a -> Interaction (WithUndo a)
withUndo (Interaction state0 handle drawI) = Interaction state0' handle' draw' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "u"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    draw' (WithUndo s _) = drawI s
    
runInteraction :: Interaction s -> IO ()
runInteraction interaction@(Interaction state handle draw) = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  write "\ESCc"
  writeln (draw state)
  interactionLoop interaction

interactionLoop :: Interaction s -> IO ()
interactionLoop (Interaction state handle draw) = do
  input <- getChar
  write "\ESCc"
  let newState = handle (KeyPress [input]) state
  writeln (draw newState)
  interactionLoop (Interaction newState handle draw)

startScreen :: Screen
startScreen = "Sokoban! Press space to start"
      
etap5 :: IO ()
etap5 = runInteraction (withStartScreen (withUndo (resettable
        (Interaction (initialCoord mazes) handleEvent draw))))