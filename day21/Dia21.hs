{-# LANGUAGE ViewPatterns #-}

module Dia21 where

import qualified Data.Map as M
import Data.Map (Map)
import Data.List.Split (splitOn, chunksOf)

type Pattern = [String]
type Book = Map Pattern Pattern
type Grid a = [[a]]

parseRule :: String -> (Pattern, Pattern)
parseRule (words -> [from, "=>", to]) = (splitOn "/" from, splitOn "/" to)
parseRule line = error $ "error parsing rule: " ++ line

flipH :: Pattern -> Pattern
flipH = fmap reverse

flipV :: Pattern -> Pattern
flipV = reverse

rotateC4 :: Pattern -> Pattern
rotateC4 [l1, l2] =
  let [l1l, l1r] = l1
      [l2l, l2r] = l2
      l1' = [l2l, l1l]
      l2' = [l2r, l1r]
  in [l1', l2']
rotateC4 [l1, l2, l3] =
  let [l1l, l1c, l1r] = l1
      [l2l, l2c, l2r] = l2
      [l3l, l3c, l3r] = l3
      l1' = [l3l, l2l, l1l]
      l2' = [l3c, l2c, l1c]
      l3' = [l3r, l2r, l1r]
  in [l1', l2', l3']

allVariants :: Pattern -> [Pattern]
allVariants patt =
  let vars = [patt, flipH patt, flipV patt]
  in concatMap (take 4 . iterate rotateC4) vars

addRule :: (Pattern, Pattern) -> Book -> Book
addRule (from, to) book =
  let all_variants = allVariants from
  in foldr (\p acc -> M.insert p to acc) book all_variants

compileBook :: [(Pattern, Pattern)] -> Book
compileBook rules = foldr (\rule book -> addRule rule book) M.empty rules

split :: Grid a -> [Grid a]
split grid =
  let
    size = length grid
    splitFactor = if mod size 2 == 0 then 2 else 3
    piecesPerLine = div size splitFactor
  in
    [ [chunk !! i | chunk <- chunked]
    | lines <- chunksOf splitFactor grid
    , let chunked = fmap (chunksOf splitFactor) lines
    , i <- [0..piecesPerLine - 1]
    ]

merge :: [Grid a] -> Grid a
merge grids =
  let outerSide = floor $ sqrt $ fromIntegral $ length grids
      innerSide = length $ grids !! 0
  in [ concat [block !! i | block <- lines]
     | lines <- chunksOf outerSide grids
     , i <- [0..innerSide - 1]
     ]

stepGrid :: Book -> Grid Char -> Grid Char
stepGrid = (M.!)

stepArt :: Book -> Grid Char -> Grid Char
stepArt book grid = merge $ stepGrid book <$> split grid

loadBook :: IO Book
loadBook = do
  rules <- (fmap parseRule) . lines <$> readFile "input.txt"
  pure (compileBook rules)

startingPattern :: Pattern
startingPattern = lines ".#.\n..#\n###"

pixelsOn :: Grid Char -> Int
pixelsOn grid = sum [1 | '#' <- concat grid]

main :: IO ()
main = do
  book <- loadBook
  let arts = iterate (stepArt book) startingPattern
      art  = arts !! 5
      art2 = arts !! 18
  print (pixelsOn art)
  print(pixelsOn art2)
