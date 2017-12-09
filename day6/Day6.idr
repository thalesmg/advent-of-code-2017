module Day6

import Data.Vect
import Data.SortedSet
import Data.SortedMap

Banks : Nat -> Type
Banks n = Vect n Int

testBanks : Banks 4
testBanks = [0,2,7,0]

findMax : Banks (S n) -> Nat
findMax = Prelude.Basics.fst . snd . foldl tracker (0, 0, -1)
  where
    tracker : (Nat, Nat, Int) -> Int -> (Nat, Nat, Int)
    tracker (curInd, maxInd, acc) x =
      if x > acc
        then (S curInd, curInd, x)
        else (S curInd, maxInd, acc)

redistribute : {auto prf : len = S n} -> Banks len -> Banks len
redistribute {len = S n} bs =
  let maxNat = findMax bs
      maxInd = restrict n (cast maxNat)
      nextInd = restrict n (cast (S maxNat))
      value = Data.Vect.index maxInd bs
      bs' = updateAt maxInd (const 0) bs
  in go nextInd value bs'
    where
      go : Fin (S m) -> Int -> Banks (S m) -> Banks (S m)
      go _ 0 bs = bs
      go {m} nextInd x bs =
        let bs' = updateAt nextInd (+1) bs
            nextInd' = restrict m (1 + finToInteger nextInd)
        in go nextInd' (x - 1) bs'

countRep : Banks (S n) -> Nat
countRep bs = go Z empty bs
  where
    go : (count : Nat) -> SortedSet (Banks (S n)) -> Banks (S n) -> Nat
    go count set bs =
      let bs' = redistribute bs
      in if contains bs' set
         then (S count)
         else go (S count) (insert bs' set) bs'

toBanks : List String -> (n ** Banks n)
toBanks lst = (_ ** fromList (map cast lst))

countRep2 : Banks (S n) -> Int
countRep2 bs = go Z empty empty (redistribute bs)
  where
    go : (count : Nat) -> SortedMap (Banks (S n)) Nat -> SortedSet (Banks (S n)) -> Banks (S n) -> Int
    go count mcount set bs =
      if contains bs set
        then
          let Just firstCount = lookup bs mcount
          in cast count - cast firstCount
        else go (S count) (insert bs count mcount) (insert bs set) (redistribute bs)

namespace Part1
  main : IO ()
  main = do
    Right raw <- readFile "input.txt"
      | Left err => print err
    let (n ** bs) = toBanks (words raw)
    case n of
      Z => print "fuck!"
      S m => do
        let res = countRep bs
        print res
        pure ()

namespace Part2
  main : IO ()
  main = do
    Right raw <- readFile "input.txt"
      | Left err => print err
    let (n ** bs) = toBanks (words raw)
    case n of
      Z => print "fuck!"
      S m => do
        let res = countRep2 bs
        print res
        pure ()

namespace Main
  main : IO ()
  main = do
    Right raw <- readFile "input.txt"
      | Left err => print err
    let (n ** bs) = toBanks (words raw)
    case n of
      Z => print "fuck!"
      S m => do
        let res = countRep2 bs
        print res
        pure ()
