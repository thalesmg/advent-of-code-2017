{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Prelude hiding (takeWhile)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Applicative
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Char (isAlpha)
import Data.Attoparsec.Text

type Name = Text
type Weight = Int

data Link = Link Name Weight [Name]
            deriving (Eq, Show)

parseLink :: Parser Link
parseLink =
  Link
  <$> name
  <*> (skipSpace *> char '(' *> decimal <* char ')')
  <*> ((skipSpace *> string "->" *> skipSpace *>
       sepBy name (string ", "))
      <|> pure [])
  where
    name :: Parser Name
    name = takeWhile isAlpha

data Node = Node Name Weight deriving (Eq, Show, Ord)

type Recipe = Map Name (Maybe Node)

buildIndex :: [Link] -> Recipe
buildIndex [] = M.empty
buildIndex ls = go M.empty ls
  where
    go :: Recipe -> [Link] -> Recipe
    go m [] = m
    go m (Link name w children : rest) =
      let m' = foldr (\child acc -> M.insert child (Just (Node name w)) acc) m children
          m'' = M.insertWith (flip const) name Nothing m'
      in go m'' rest

data Tree = Tree Node [Node] deriving (Eq, Show)

buildTree :: Recipe -> Tree
buildTree recipe =
  let [root] = M.toList $ M.filter isNothing recipe
  in undefined

main :: IO ()
main = do
  contents <- TIO.readFile "input.txt"
  let Right parsed = parseOnly (many1 (parseLink <* endOfLine)) contents
      ind = buildIndex parsed
      parentless = M.filter isNothing ind
  print parentless
  pure ()
