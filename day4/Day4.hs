module Day4 where

import qualified Data.Set as S
import qualified Data.Map as M

isValid :: [String] -> Bool
isValid = go S.empty
  where
    go :: S.Set String -> [String] -> Bool
    go set [] = True
    go set (x:xs) = not (S.member x set) && go (S.insert x set) xs

isValid2 :: [String] -> Bool
isValid2 = go S.empty
  where
    go :: S.Set (M.Map Char Int) -> [String] -> Bool
    go set [] = True
    go set (x:xs) =
      let ax = analyze x
      in not (S.member ax set) && go (S.insert ax set) xs

    analyze :: String -> M.Map Char Int
    analyze = foldr (\c acc -> M.insertWith (+) c 1 acc) M.empty

main :: IO ()
main = do
  contents <- fmap words . lines <$> readFile "input.txt"
  let res = length . filter id $ map isValid contents
  let res2 = length . filter id $ map isValid2 contents
  print res
  print res2
