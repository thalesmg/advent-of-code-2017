module Main where

import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.List.Split (splitOn)
import Data.Foldable (foldl')
import Data.List (sortOn)

numProgs :: Int
numProgs = 16

data State = State { byIndex :: Map Int Char
                   , byName  :: Map Char Int
                   }
             deriving (Eq, Show)

data Command = Spin Int
             | Exchange Int Int
             | Partner Char Char
             deriving (Eq, Show)

parseCommand :: String -> Command
parseCommand ('s' : num)  = Spin (read num)
parseCommand ('x' : nums) = let (num1, '/' : num2) = break (== '/') nums
                            in Exchange (read num1) (read num2)
parseCommand ('p' : nums) = let ([char1], '/' : char2 : _) = break (== '/') nums
                            in Partner char1 char2
parseCommand _ = error "parse error!"

execute :: State -> Command -> State
execute st (Spin n) =
  let
    spin m = mod (m + n) numProgs
    byName' = spin <$> byName st
    byIndex' = M.mapKeys spin (byIndex st)
  in
    State {byIndex = byIndex', byName = byName'}
execute State{byIndex = bi, byName = bn} (Exchange n m) =
  let
    old_n_prog = bi ! n
    old_m_prog = bi ! m
    byName'  = foldr (\(k, v) acc -> M.insert k v acc) bn [(old_m_prog, n), (old_n_prog, m)]
    byIndex' = foldr (\(k, v) acc -> M.insert k v acc) bi [(n, old_m_prog), (m, old_n_prog)]
  in
    State{byName = byName', byIndex = byIndex'}
execute State{byIndex = bi, byName = bn} (Partner p1 p2) =
  let
    old_p1_ind = bn ! p1
    old_p2_ind = bn ! p2
    byName'  = foldr (\(k, v) acc -> M.insert k v acc) bn [(p2, old_p1_ind), (p1, old_p2_ind)]
    byIndex' = foldr (\(k, v) acc -> M.insert k v acc) bi [(old_p1_ind, p2), (old_p2_ind, p1)]
  in
    State{byName = byName', byIndex = byIndex'}

loadCommands :: IO [Command]
loadCommands = fmap parseCommand . splitOn "," <$> readFile "input.txt"

initialState :: State
initialState =
  let
    bn = M.fromList $ zip ['a'..'p'] [0..numProgs - 1]
    bi = M.fromList $ zip [0..numProgs - 1] ['a'..'p']
  in
    State{byName = bn, byIndex = bi}

swap :: Ord k => k -> k -> Map k v -> Map k v
swap k1 k2 m =
  let
    old_v_k1 = m ! k1
    old_v_k2 = m ! k2
  in
    foldr (\(k, v) acc -> M.insert k v acc) m [(k1, old_v_k2), (k2, old_v_k1)]

toPermutation :: State -> State -> Int -> Int
toPermutation State{byName = bn1, byIndex = bi1} State{byName = bn2} x =
  let
    old_char = bi1 ! x
    old_position = bn1 ! old_char
    new_position = bn2 ! old_char
  in
    new_position

applyPermutationNTimes :: (Int -> Int) -> Int -> (Int -> Int)
applyPermutationNTimes p 0 = id
applyPermutationNTimes p 1 = p
applyPermutationNTimes p n = p . applyPermutationNTimes p (n - 1)

-- findIdentity :: (Int -> Int) -> Int
-- findIdentity p = go p original 0
--   where
--     original = [0..numProgs - 1]

--     go :: (Int -> Int) -> [Int] -> Int -> Int
--     go p lst count = count `seq`
--       let
--         lst' = map p lst
--       in
--         if map p lst == original
--         then count + 1
--         else go p lst' (count + 1)

findIdentity :: [Command] -> State -> Int
findIdentity cs st = go st 0
  where
    original = st

    go :: State -> Int -> Int
    go state count = count `seq`
      let
        state' = foldl execute state cs
      in
        if state' == original
        then count + 1
        else go state' (count + 1)


main :: IO ()
main = do
  commands <- loadCommands
  let result1 = foldl execute initialState commands
  print (M.elems (byIndex result1))
  print (M.elems (byName result1))
  let permutation = toPermutation initialState result1
      idCount = findIdentity commands initialState
      realApplicationNumber = mod 1000000000 idCount
      result2 = foldl execute initialState (concat $ replicate realApplicationNumber commands)
  print idCount
  print result2
  print idCount
  print realApplicationNumber
  print (M.elems (byIndex result2))
  -- print "fuck"
  -- print (map permutation [0..numProgs - 1])
  -- mapM_ (putStr . (:[]) . fst) $ sortOn snd (zip ['a'..'p'] (map permutation [0..numProgs - 1]))
  -- putStrLn ""
  -- print "fuck"
  -- print (map (applyPermutationNTimes permutation 12) [0..numProgs - 1])
  -- print (map (applyPermutationNTimes permutation 13) [0..numProgs - 1])
  -- print (map (applyPermutationNTimes permutation 4) [0..numProgs - 1])
  -- mapM_ (putStr . (:[]) . fst) $ sortOn snd (zip ['a'..'p'] result2)
  -- putStrLn ""
  -- print (map (applyPermutationNTimes permutation 5) [0..numProgs - 1])
  -- mapM_ (putStr . (:[]) . fst) $ sortOn snd (zip ['a'..'p'] (map (applyPermutationNTimes permutation 5) [0..numProgs - 1]))
  putStrLn ""
