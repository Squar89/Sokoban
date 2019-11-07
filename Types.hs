module Types where

type Program = IO ()
type MazeT = Coord -> Tile
type DrawFun = Integer -> Integer -> Char
type Picture = DrawFun -> DrawFun
type Screen = String

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
data Direction = R | U | L | D deriving Eq
data Coord = C Integer Integer deriving Eq
data SSState world = StartScreen | Running world deriving Eq
data State = S Coord Direction [Coord] [Maze] Integer
data Interaction world = Interaction
    world
    (Event -> world -> world)
    (world -> Screen)
data Event = KeyPress String
data WithUndo a = WithUndo a [a]
data Maze = Maze Coord (Coord -> Tile)

instance Eq State where
  (S c d l _ cnt) == (S c' d' l' _ cnt') = (c == c') && (d == d') && (l == l') && (cnt == cnt')

(&) = (.)
