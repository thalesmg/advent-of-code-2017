{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Char (isAlpha)
import Data.List (stripPrefix)
import Data.Map (Map)
import qualified Data.Map as M

newtype Register = Register Char deriving (Eq, Show, Ord)

data Value = Pointer Register
           | Primitive Int
           deriving (Eq, Show)

data Instruction where
  Snd :: Register -> Instruction
  Set :: Register -> Value -> Instruction
  Add :: Register -> Value -> Instruction
  Mul :: Register -> Value -> Instruction
  Mod :: Register -> Value -> Instruction
  Rcv :: Register -> Instruction
  Jgz :: Register -> Value -> Instruction
  deriving (Eq, Show)

type Zipper a = ([a], a, [a])
type Program = Zipper Instruction
type Processor = Map Register Int
data State = Running (Program, Processor, Maybe Int)
           | Finished Int
           deriving (Eq, Show)

get :: Processor -> Register -> Int
get processor reg = M.findWithDefault 0 reg processor

put :: Processor -> Register -> Int -> Processor
put processor reg x = M.insert reg x processor

deref :: Processor -> Value -> Int
deref _ (Primitive x) = x
deref processor (Pointer reg) = get processor reg

move :: Int -> Zipper a -> Zipper a
move n prog@(prev, inst, next)
  | n == 0    = prog
  | n >  0    = move (n - 1) (inst : prev, head next, tail next) -- (take (n - 1) next ++ inst : prev, next !! (n - 1), drop n next)
  | otherwise = move (n + 1) (tail prev, head prev, inst : next) -- (drop n' prev, prev !! (n' - 1), take n' prev ++ inst : next)

execute1 :: State -> State
execute1 (Running (prog@(_, inst, _), processor, msound)) =
  case inst of
    Snd reg ->
      Running (move 1 prog, processor, Just (get processor reg))
    Set reg val ->
      Running (move 1 prog, put processor reg (deref processor val), msound)
    Add reg val ->
      Running (move 1 prog, put processor reg (deref processor val + get processor reg), msound)
    Mul reg val ->
      Running (move 1 prog, put processor reg (deref processor val * get processor reg), msound)
    Mod reg val ->
      Running (move 1 prog, put processor reg (deref processor val `mod` get processor reg), msound)
    Rcv reg ->
      if get processor reg /= 0
      then case msound of
             Just freq -> Finished freq
             _         -> Finished (-999)
      else Running (move 1 prog, processor, msound)
    Jgz reg val ->
      let
        reg_val = get processor reg
      in
        if reg_val > 0
        then Running (move (deref processor val) prog, processor, msound)
        else Running (move 1 prog, processor, msound)
execute1 (Finished x) = Finished x

execute :: State -> Int
execute state =
  case execute1 state of
    state'@(Running _) -> execute state'
    Finished x -> x

parseVal :: String -> Value
parseVal raw =
  case raw of
    [reg] | isAlpha reg -> Pointer (Register reg)
    _ -> Primitive (read raw)

parse1 :: String -> Instruction
parse1 (stripPrefix "snd " -> Just (words -> [[reg]])) = Snd (Register reg)
parse1 (stripPrefix "rcv " -> Just (words -> [[reg]])) = Rcv (Register reg)
parse1 (stripPrefix "set " -> Just (words -> [[reg], raw])) = Set (Register reg) (parseVal raw)
parse1 (stripPrefix "add " -> Just (words -> [[reg], raw])) = Add (Register reg) (parseVal raw)
parse1 (stripPrefix "mul " -> Just (words -> [[reg], raw])) = Mul (Register reg) (parseVal raw)
parse1 (stripPrefix "mod " -> Just (words -> [[reg], raw])) = Mod (Register reg) (parseVal raw)
parse1 (stripPrefix "jgz " -> Just (words -> [[reg], raw])) = Jgz (Register reg) (parseVal raw)

main :: IO ()
main = do
  instructions@(inst : rest) <- fmap parse1 . lines <$> readFile "input.txt"
  let initial = Running (([], inst, rest), M.empty, Nothing)
      result1 = execute initial
  print initial
  print result1
  pure ()
