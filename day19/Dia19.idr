module Dia19

import Data.SortedMap

%default total

Coord : Type
Coord = (Int, Int)

data Piece : Type where
  Verti : Piece
  Horiz : Piece
  Cross : Piece
  Lette : Char -> Piece

Eq Piece where
  (==) Verti Verti = True
  (==) Horiz Horiz = True
  (==) Cross Cross = True
  (==) (Lette x) (Lette y) = x == y
  (==) _ _ = False

Tubes : Type
Tubes = SortedMap Coord Piece

parsePiece : Char -> Maybe Piece
parsePiece '|' = Just Verti
parsePiece '-' = Just Horiz
parsePiece '+' = Just Cross
parsePiece  c  =
  case elem c ['A'..'Z'] of
    True => Just $ Lette c
    False => Nothing

parsePieces : List (List Char) -> List (Coord, Piece)
parsePieces contents = go 0 contents []
  where
    goLine : Int -> Int -> List Char -> List (Coord, Piece) -> List (Coord, Piece)
    goLine i j [] acc = acc
    goLine i j (c::cs) acc =
      case parsePiece c of
        Just piece => goLine i (j + 1) cs (((i, j), piece) :: acc)
        Nothing => goLine i (j + 1) cs acc

    go : Int -> List (List Char) -> List (Coord, Piece) -> List (Coord, Piece)
    go i [] acc = acc
    go i (l::ls) acc = go (i + 1) ls (goLine i 0 l acc)

loadTubes : IO Tubes
loadTubes = do
  Right contents <- map (map unpack . lines) <$> readFile "input.txt"
    | Left err => do
      print err
      pure empty
  let pieces = parsePieces contents
  pure (fromList pieces)

findEntrance : Tubes -> Maybe Coord
findEntrance tubes =
  case filter (\((i, j), v) => i == 0 && v == Verti) . toList $ tubes of
    [] => Nothing
    ((c, _)::_) => Just c

data Direction = Left | Down | Right | Up

Eq Direction where
  (==) Left Left = True
  (==) Down Down = True
  (==) Right Right = True
  (==) Up Up = True
  (==) _ _ = False

Show Direction where
  show Left = "Left"
  show Down = "Down"
  show Right = "Right"
  show Up = "Up"

-- State : Type
-- State = (Coord, Direction, List Char)

record State where
  constructor MkState
  coord : Coord
  dir : Direction
  letters : List Char
  steps : Int

data Walker = Walking State
            | Finished State
            | Failed State

nextCoord : Coord -> Direction -> Coord
nextCoord (i, j) dir =
  case dir of
    Left => (i, j - 1)
    Down => (i + 1, j)
    Right => (i, j + 1)
    Up => (i - 1, j)

opposite : Direction -> Direction
opposite Left = Right
opposite Down = Up
opposite Right = Left
opposite Up = Down

neighbors : Coord -> Direction -> List (Direction, Coord)
neighbors current dir =
  -- let next = nextCoord (i, j) dir
  -- in [(i', j') | i' <- [(i - 1)..(i + 1)], j' <- [(j - 1)..(j + 1)], (i', j') /= next, abs (i' + j' - i - j) == 1]
  [(dir', neigh) | dir' <- [Left, Down, Right, Up], dir' /= (opposite dir), let neigh = nextCoord current dir', neigh /= (current)]

nextInCross : Tubes -> Coord -> Direction -> Maybe (Direction, Coord)
nextInCross tubes current direc =
  let neighs = neighbors current direc
      nexts = map (\(direc, neigh) => ((direc, neigh), lookup neigh tubes)) neighs
      filtered = the (List ((Direction, Coord), Maybe Piece)) (filter (isJust . Prelude.Basics.snd) nexts)
  in case filtered of
       [] => Nothing
       ((x, _) :: xs) => Just x

stepMaze : Tubes -> Walker -> Walker
stepMaze tubes (Walking state) =
  case lookup (coord state) tubes of
    Nothing => Failed state
    (Just current) =>
      case current of
        Cross => case nextInCross tubes (coord state) (dir state) of
                   Nothing => Failed state
                   (Just (dir', next')) => Walking (record {coord = next', dir = dir', steps $= (+1)} state)
        (Lette l) => Walking (record {coord = nextCoord (coord state) (dir state), letters $= (l::), steps $= (+1)} state)
        _ => Walking (record {coord = nextCoord (coord state) (dir state), steps $= (+1)} state)
stepMaze _ walker = walker

partial
walkMaze : Tubes -> Walker -> State
walkMaze tubes walker =
  case stepMaze tubes walker of
    walker'@(Walking x) => assert_smaller walker (walkMaze tubes walker')
    (Finished state) => state
    (Failed state) => state

namespace Parte1
  partial
  main : IO ()
  main = do
    tubes <- loadTubes
    let entrance = findEntrance tubes
    case entrance of
      Just entrance => do
        let final = walkMaze tubes (Walking (MkState entrance Down [] 0))
        printLn . pack . reverse . letters $ final
        printLn . steps $ final
      Nothing => print "Não achei a entrada!!!!!!!!"

namespace Main
  partial
  main : IO ()
  -- main = do
  --   tubes <- loadTubes
  --   print (nextInCross tubes (5,8) Right)
  main = Parte1.main
