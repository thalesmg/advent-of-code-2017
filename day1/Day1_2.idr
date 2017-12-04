module Day1_2

import Data.Vect
import Data.Fin -- for restrict
import Data.Nat.Views

parseDigit : Char -> Int
parseDigit = cast . singleton

getNext : (curIndex : Nat) -> (vect : Vect (S (S n)) Int) -> Int
getNext curIndex {n} vect =
  let steps = div (S (S n)) 2
      nextIndex = restrict (S n) (toIntegerNat (curIndex + steps))
  in Data.Vect.index nextIndex vect

resolve : Vect (S (S n)) Int -> Int
resolve v = go (0, 0) v
  where
    go : (Nat, Int) -> Vect (S (S n)) Int -> Int
    go (i, acc) {n} v =
      if i >= (S (S n))
        then acc
        else let next = getNext i v
                 fin_i = case natToFin i (S (S n)) of
                              Just f => f
                              Nothing => FZ
                 last = Data.Vect.index fin_i v
             in if next == last
                then go (S i, acc + next) v
                else go (S i, acc) v

toVect : String -> (n ** Vect n Int)
toVect str = magic (map parseDigit (unpack str)) []
  where
    magic : List a -> Vect m a -> (n ** Vect n a)
    magic [] {m} ys = (m ** reverse ys)
    magic (x :: xs) ys = magic xs (x :: ys)

main : IO ()
main = do
  Right contents <- readFile "input.txt"
    | Left err => putStrLn ("Error opening input!! : " ++ show err)
  -- let converted = map parseDigit (fromList (unpack contents))
  -- putStrLn (show (unpack contents))
  -- putStrLn (show ((map parseDigit (unpack contents))))
  case toVect (trim contents) of
    (S (S n) ** v) => do
      putStrLn (show (last v))
      putStrLn (show (resolve v))
    (x ** pf) => putStrLn "Kabum!!!!"
