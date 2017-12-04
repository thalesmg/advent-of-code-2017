module Day1 where

type State a = Integer -> (a, Integer)

firstBreak :: [Integer] -> Maybe Int
firstBreak [] = Nothing
firstBreak (x:xs) = go (0, x) xs
  where
    go :: (Int, Integer) -> [Integer] -> Maybe Int
    go _ [] = Nothing
    go (i, fstval) (x:xs) = if x == fstval
                            then go (i+1, fstval) xs
                            else Just (i + 1)

normalize :: [Integer] -> [Integer]
normalize lst =
  case firstBreak lst of
    Nothing ->
