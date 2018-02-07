--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Dia22 where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
-- import Control.Lens

type Coord = (Int, Int)
type Grid a = Map Coord a
data Dir = D | U | L | R deriving (Eq, Show)

type Lens' a b = forall f. Functor f => (b -> f b) -> (a -> f a)

data Virus = Virus { _dir :: Dir
                   , _pos :: Coord
                   }
             deriving (Eq, Show)
dir :: Lens' Virus Dir
dir k v = fmap (\d' -> v {_dir = d'}) (k (_dir v))
pos :: Lens' Virus Coord
pos k v = fmap (\p' -> v {_pos = p'}) (k (_pos v))
-- makeLenses ''Virus

data NodeStatus = Clean
                | Weakened
                | Infected
                | Flagged
                deriving (Eq, Show)

newtype Const a b = Const {getConst :: a}
instance Functor (Const a) where
  fmap f (Const x) = Const x

newtype Identity a = Identity {getIdentity :: a}
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

over :: Lens' a b -> (b -> b) -> a -> a
-- over lens f obj = getConst $ lens (const (fmap f (Const obj))) obj
over lens f obj = getIdentity $ lens (\b -> Identity (f b)) obj

set :: Lens' a b -> b -> a -> a
set lens b' obj = over lens (\_ -> b') obj

view :: Lens' a b -> a -> b
view lens obj = getConst $ lens (\b -> Const b) obj

type Stats = Int
type Stats' = [Coord]

turnLeft :: Dir -> Dir
turnLeft D = R
turnLeft R = U
turnLeft U = L
turnLeft L = D

turnRight :: Dir -> Dir
turnRight D = L
turnRight L = U
turnRight U = R
turnRight R = D

reverseDir :: Dir -> Dir
reverseDir D = U
reverseDir U = D
reverseDir L = R
reverseDir R = L

turnVirus :: Bool -> Virus -> Virus
turnVirus True = over dir turnRight
turnVirus False = over dir turnLeft

turnVirus' :: NodeStatus -> Virus -> Virus
turnVirus' status v =
  case status of
    Clean -> over dir turnLeft v
    Weakened -> v
    Infected -> over dir turnRight v
    Flagged -> over dir reverseDir v

mutateNode :: NodeStatus -> NodeStatus
mutateNode status =
  case status of
    Clean -> Weakened
    Weakened -> Infected
    Infected -> Flagged
    Flagged -> Clean

move :: Coord -> Dir -> Coord
move (x, y) U = (x, y - 1)
move (x, y) L = (x - 1, y)
move (x, y) D = (x, y + 1)
move (x, y) R = (x + 1, y)

moveVirus :: Virus -> Virus
moveVirus virus =
  let newPos = move (view pos virus) (view dir virus)
  in set pos newPos virus

stepVirus :: (Stats, Grid Bool, Virus) -> (Stats, Grid Bool, Virus)
stepVirus (stats, grid, virus) =
  let
    currentNodeStatus = M.findWithDefault False (view pos virus) grid
    grid' = M.insert (view pos virus) (not currentNodeStatus) grid
    virus' = moveVirus . turnVirus currentNodeStatus $ virus
    stats' = if currentNodeStatus then stats else stats + 1
  in
    seq stats' (stats', grid', virus')

stepVirus' :: (Int, Grid NodeStatus, Virus) -> (Int, Grid NodeStatus, Virus)
-- stepVirus' :: (Stats', Grid NodeStatus, Virus) -> (Stats', Grid NodeStatus, Virus)
stepVirus' (stats, grid, virus) =
  let
    currentNodeStatus = M.findWithDefault Clean (view pos virus) grid
    grid' = M.insert (view pos virus) (mutateNode currentNodeStatus) grid
    virus' = moveVirus . turnVirus' currentNodeStatus $ virus
    -- stats' = if currentNodeStatus /= Weakened then stats else S.insert (view pos virus) stats
    -- stats' = if currentNodeStatus /= Weakened then stats else view pos virus : stats
    stats' = if currentNodeStatus /= Weakened then stats else stats + 1
  in
    seq stats' (stats', grid', virus')

parseGrid :: [String] -> Grid Bool
parseGrid ls = foldl (\m (j, l) -> foldl (insertNode j) m (zip [0..] l)) M.empty (zip [0..] ls)
  where
    insertNode :: Int -> Grid Bool -> (Int, Char) -> Grid Bool
    insertNode j m (i, c) =
      if isInfected c
      then M.insert (i, j) (isInfected c) m
      else m

    isInfected :: Char -> Bool
    isInfected '#' = True
    isInfected  _  = False

parseGrid' :: [String] -> Grid NodeStatus
parseGrid' ls = foldl (\m (j, l) -> foldl (insertNode j) m (zip [0..] l)) M.empty (zip [0..] ls)
  where
    insertNode :: Int -> Grid NodeStatus -> (Int, Char) -> Grid NodeStatus
    insertNode j m (i, c) =
      if isInfected c
      then M.insert (i, j) Infected m
      else m

    isInfected :: Char -> Bool
    isInfected '#' = True
    isInfected  _  = False

startingPoint :: [String] -> Coord
startingPoint ls = (i, j)
  where
    (m, n) = (length ls, length . head $ ls)
    (i, j) = (div m 2, div n 2)

loadGrid :: IO (Coord, Grid Bool)
loadGrid = do
  contents <- lines <$> readFile "input.txt"
  let start = startingPoint contents
      grid = parseGrid contents
  pure (start, grid)

loadGrid' :: IO (Coord, Grid NodeStatus)
loadGrid' = do
  contents <- lines <$> readFile "input.txt"
  let start = startingPoint contents
      grid = parseGrid' contents
  pure (start, grid)

main :: IO ()
main = do
  (start, grid) <- loadGrid
  let virus = Virus {_pos = start, _dir = U}
      stats = 0
      (stats', _, _) = iterate stepVirus (stats, grid, virus) !! 10000
  print stats'
  (start, grid') <- loadGrid'
  -- let blacklist = M.foldrWithKey (\p n s -> if n == Infected then S.insert p s else s) S.empty grid'
  -- print blacklist
  -- let (stats'', _, _) = iterate stepVirus' ([], grid', virus) !! 100
  -- print $ length stats''
  -- let (stats'', _, _) = iterate stepVirus' (0, grid', virus) !! 10000000
  let virus = Virus {_pos = start, _dir = U}
      (stats'', _, _) = iterate stepVirus' (0, grid', virus) !! 10000000
  print stats''
