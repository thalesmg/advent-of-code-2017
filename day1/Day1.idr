module Day1

import Data.Vect

%default total

firstBreak : List Int -> Maybe Nat
firstBreak [] = Nothing
firstBreak (x::xs) = go (0, x) xs
  where
    go : (Nat, Int) -> List Int -> Maybe Nat
    go _ [] = Nothing
    go (i, fstval) (x::xs) = if x == fstval
                                then go (S i, fstval) xs
                                else Just (S i)

normalize : List Int -> List Int
normalize lst = case firstBreak lst of
                           Nothing => lst
                           (Just ind) => let prefixo = take ind lst
                                             resto = drop ind lst
                                         in resto ++ prefixo

State : Type
State = (Nat, Int, Int)

add : Int -> State -> State
add digit (count, lastDigit, acc) = if digit == lastDigit
                                       then (S count, lastDigit, acc)
                                       else (Z, digit, acc + (cast count) * lastDigit)

finalSum : List Int -> Int
finalSum [] = 0
finalSum (x :: xs) = go (Z, x, 0) xs
  where
    go : State -> List Int -> Int
    go (count, lastDigit, acc) [] = acc + (cast count) * lastDigit
    go (count, lastDigit, acc) (digit :: xs) =
      let state = if digit == lastDigit
                     then (S count, lastDigit, acc)
                     else (Z, digit, acc + (cast count) * lastDigit)
      in go state xs

finalSum2 : List Int -> Int
finalSum2 [] = 0
finalSum2 (x :: xs) = go (x, x, 0) xs
  where
    go : (Int, Int, Int) -> List Int -> Int
    go (first, lastDigit, acc) [] = if first == lastDigit
                                       then acc + first
                                       else acc
    go (first, lastDigit, acc) (digit :: xs) =
      let state = if digit == lastDigit
                     then (first, lastDigit, acc + lastDigit)
                     else (first, digit, acc)
      in go state xs


parseDigit : Char -> Int
parseDigit = cast . singleton

main : IO ()
main = do
  Right contents <- readFile "input.txt"
    | Left err => putStrLn ("Error opening input!! : " ++ show err)
  let converted = map parseDigit (unpack contents)
  let normalized = normalize converted
  let result = finalSum2 normalized
  putStrLn (show result)
