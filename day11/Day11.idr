module Day11

%default total

data Dir = SW | S | SE | NE | N | NW

-- ||| x aumenta para S, y aumenta para SE
-- |||
-- |||   \ n  /
-- ||| nw +--+ ne
-- |||   /    \
-- ||| -+      +-
-- |||   \    /
-- ||| sw +--+ se
-- |||   / s  \
-- |||
Coord : Type
Coord = (Int, Int)

move : Coord -> Dir -> Coord
move (x, y) SW = (x + 1, y - 1)
move (x, y) S = (x + 1, y)
move (x, y) SE = (x, y + 1)
move (x, y) NE = (x - 1, y + 1)
move (x, y) N = (x - 1, y)
move (x, y) NW = (x, y - 1)

partial
parseDir : String -> Dir
parseDir "se" = SE
parseDir "sw" = SW
parseDir "s"  = S
parseDir "ne" = NE
parseDir "nw" = NW
parseDir "n"  = N

dec : Int -> Int
dec x with (x >= 0)
  | True = x - 1
  | False = x + 1

partial
steps : Coord -> Int
steps coord = go 0 coord
  where
    partial
    go : Int -> Coord -> Int
    go acc (n, 0) = acc + abs n
    go acc (0, n) = acc + abs n
    go acc (n, m) =
      if n * m > 0
        then if n > m
                then go (acc + 1) (dec n, m)
                else go (acc + 1) (n, dec m)
        else go (acc + 1) (dec n, dec m)

namespace Part1
  partial
  main : IO ()
  main = do
    Right contents <- readFile "input.txt"
      | Left err => printLn err
    let dirs = map parseDir (split (== ',') (trim contents))
    let last_coord = foldl move (0, 0) dirs
    let resp = steps last_coord
    printLn resp
    pure ()


namespace Part2
  partial
  move' : (Int, Coord) -> Dir -> (Int, Coord)
  move' (recorde, pos) dir =
    let pos' = move pos dir
    in (max recorde (steps pos'), pos')

  partial
  main : IO ()
  main = do
    Right contents <- readFile "input.txt"
      | Left err => printLn err
    let dirs = map parseDir (split (== ',') (trim contents))
    let (recorde, last_coord) = foldl move' (0, (0, 0)) dirs
    let resp = steps last_coord
    printLn resp
    printLn recorde
    pure ()
