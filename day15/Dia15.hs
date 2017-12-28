module Main where

import Data.Bits
import Data.Word (Word64)

factor_a :: Word64
factor_a = 16807

factor_b :: Word64
factor_b = 48271

divisor :: Word64
divisor = 2147483647

initial_a :: Word64
initial_a = 516

initial_b :: Word64
initial_b = 190

last16bitsMask :: Word64
last16bitsMask = 0xffff

last16zeros :: Word64
last16zeros = 0x0000

produce :: Word64 -> Word64 -> Word64 -> Word64
produce factor divisor previous =
  rem (previous * factor) divisor

generator_a :: [Word64]
generator_a = go initial_a
  where
    go :: Word64 -> [Word64]
    go prev = let x = produce factor_a divisor prev
              in x : go x

generator_b :: [Word64]
generator_b = go initial_b
  where
    go :: Word64 -> [Word64]
    go prev = let x = produce factor_b divisor prev
              in x : go x

judge :: Word64 -> Word64 -> Bool
judge x y = (xor x y) .&. last16bitsMask == last16zeros

judgeNtimes :: Int -> Integer
judgeNtimes n = go n 0 generator_a generator_b
  where
    go :: Int -> Integer -> [Word64] -> [Word64] -> Integer
    go 0 acc _ _ = acc
    go n acc (a : as) (b : bs) =
      if judge a b
        then go (pred n) (acc + 1) as bs
        else go (pred n) acc as bs

main :: IO ()
main = do
  let res = judgeNtimes 40000000
  print res
  pure ()
