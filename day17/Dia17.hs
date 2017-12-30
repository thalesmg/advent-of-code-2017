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
    go n (size, pos, acc) =
      let
        nextIndex = 1 + mod (pos + inputSteps) size
        state' = (size + 1, seq nextIndex nextIndex, if nextIndex == 1 then size else acc)
      in
        seq state' (go (n - 1) state')

-- import qualified Data.IntMap as M
-- import Data.IntMap (IntMap, (!))

-- type Buffer = (Int, IntMap Int)

-- insertMany :: [(Int, v)] -> IntMap v -> IntMap v
-- insertMany lst m = foldr (\(k, v) acc -> M.insert k v acc) m lst

-- spin :: Int -> Buffer -> Buffer
-- spin steps (size, buffer) =
--   let
--     nextPos = mod (size + steps) size
--     lastPred = buffer ! nextPos
--   in
--     (succ size, insertMany [(nextPos, size), (size, lastPred)] buffer)

-- initial :: Buffer
-- initial = (1, M.singleton 0 0)

-- spin' = spin 3

main :: IO ()
main = print $ solution2 50000000
