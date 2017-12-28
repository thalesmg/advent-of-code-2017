module Dia15

import Data.Bits

%default total

factor_a : Bits 32
factor_a = intToBits 16807

factor_b : Bits 32
factor_b = intToBits 48271

divisor : Bits 32
divisor = intToBits 2147483647

initial_a : Bits 32
initial_a = intToBits 516

initial_b : Bits 32
initial_b = intToBits 190

last16bitsMask : Bits 32
last16bitsMask = intToBits 0xffff

last16zeros : Bits 32
last16zeros = intToBits 0x0000

partial
produce : Bits 32 -> Bits 32 -> Bits 32 -> Bits 32
produce factor divisor previous =
  urem (times previous factor) divisor

partial
generator_a : Stream (Bits 32)
generator_a = go initial_a
  where
    partial
    go : Bits 32 -> Stream (Bits 32)
    go prev = let x = produce factor_a divisor prev
              in x :: go x

partial
generator_b : Stream (Bits 32)
generator_b = go initial_b
  where
    partial
    go : Bits 32 -> Stream (Bits 32)
    go prev = let x = produce factor_b divisor prev
              in x :: go x

judge : Bits 32 -> Bits 32 -> Bool
judge x y = xor (xor x y) last16bitsMask == last16zeros

partial
judgeNtimes : Nat -> Integer
judgeNtimes n = go n 0 generator_a generator_b
  where
    go : Nat -> Integer -> Stream (Bits 32) -> Stream (Bits 32) -> Integer
    go Z acc _ _ = acc
    go (S n) acc (a :: as) (b :: bs) =
      if judge a b
        then go n (acc + 1) as bs
        else go n acc as bs

namespace Parte1
  partial
  main : IO ()
  main = do
    let res = judgeNtimes 40000000
    printLn res
    pure ()


namespace Main
  partial
  main : IO ()
  main = Parte1.main
