{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Day8 where

import Prelude hiding (takeWhile)
import Control.Applicative
import Control.Monad.Free
import qualified Data.Map as M
import Data.Map (Map)
import Data.Char (isAlpha)
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Register = Text

data CompOp = Equal
            | Less
            | Greater
            | GreaterEq
            | LessEq
            | Different
            deriving (Eq, Show)

data Op = Inc | Dec deriving (Eq, Show)

data Condition = Condition Register CompOp Int deriving (Eq, Show)

data Command = Command Register Op Int Condition deriving (Eq, Show)

op :: Parser Op
op = (string "inc" *> pure Inc)
     <|> (string "dec" *> pure Dec)

compop :: Parser CompOp
compop = (string "==" *> pure Equal)
         <|> (string ">=" *> pure GreaterEq)
         <|> (string "<=" *> pure LessEq)
         <|> (string "!=" *> pure Different)
         <|> (string ">" *> pure Greater)
         <|> (string "<" *> pure Less)

condition :: Parser Condition
condition =
  Condition
  <$> (string "if" *> skipSpace *> register)
  <*> (skipSpace *> compop)
  <*> (skipSpace *> signed decimal)

register :: Parser Register
register = takeWhile isAlpha

command :: Parser Command
command =
  Command
  <$> (register <* skipSpace)
  <*> (op <* skipSpace)
  <*> (signed decimal <* skipSpace)
  <*> condition

type Registers = Map Register Int

compOpToFun :: (Num a, Eq a, Ord a) => CompOp -> (a -> a -> Bool)
compOpToFun Equal = (==)
compOpToFun Different = (/=)
compOpToFun Less = (<)
compOpToFun Greater = (>)
compOpToFun GreaterEq = (>=)
compOpToFun LessEq = (<=)

opToFun :: (Num a, Eq a, Ord a) => Op -> (a -> a -> a)
opToFun Inc = (+)
opToFun Dec = (-)

run :: Registers -> Command -> Registers
run registers (Command reg op value (Condition compreg compop compvalue)) =
  let op' = opToFun op
      compop' = compOpToFun compop
      reg' = M.findWithDefault 0 reg registers
      compreg' = M.findWithDefault 0 compreg registers
  in
    if compop' compreg' compvalue
    then M.insert reg (op' reg' value) registers
    else registers

run2 :: (Int, Registers) -> Command -> (Int, Registers)
run2 (record, registers) (Command reg op value (Condition compreg compop compvalue)) =
  let op' = opToFun op
      compop' = compOpToFun compop
      reg' = M.findWithDefault 0 reg registers
      compreg' = M.findWithDefault 0 compreg registers
      res = op' reg' value
  in
    if compop' compreg' compvalue
    then (max res record, M.insert reg res registers)
    else (record, registers)

getCommands :: IO [Command]
getCommands = do
  contents <- T.lines <$> TIO.readFile "input.txt"
  print contents
  let Right commands = traverse (parseOnly command) contents
  -- let Right commands = parseOnly (many1 (command <* endOfLine)) contents
  pure commands

resolve :: IO Registers
resolve = do
  commands <- getCommands
  pure (foldl run M.empty commands)

resolve2 :: IO (Int, Registers)
resolve2 = do
  commands <- getCommands
  pure (foldl run2 (0, M.empty) commands)

main :: IO ()
main = do
  (record, finalRegisters) <- resolve2
  let values = M.elems finalRegisters
      result = maximum values
  print result
  print record
  pure ()
