{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Sequence as S
import Data.Sequence (Seq)

type Buffer = (Int, Int, Seq Int)

spin :: Int -> Buffer -> Buffer
spin steps (size, pos, buf) =
  let
    nextIndex = 1 + mod (pos + steps) size
  in
    (succ size, nextIndex, S.insertAt nextIndex size buf)

initial :: Buffer
initial = (1, 0, S.singleton 0)

inputSteps :: Int
inputSteps = 363

solution1 :: Maybe Int
solution1 =
  let
    (_, _, final) = iterate (spin inputSteps) initial !! 2017
  in
    S.findIndexR (== 2017) final

solution2 :: Int -> Int
solution2 n = go n (1, 0, 0)
  where
    go :: Int -> (Int, Int, Int) -> Int
    go 0 (_, _, acc) = acc
    go n (size, pos, !acc) =
      let
        nextIndex = 1 + mod (pos + inputSteps) size
        state' = (size + 1, nextIndex, if nextIndex == 1 then size else acc)
      in
        go (n - 1) state'

main :: IO ()
main = print $ solution2 50000000
