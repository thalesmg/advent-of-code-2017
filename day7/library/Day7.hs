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

type NodeInfo = (Maybe Node, [Name]) -- (Maybe Self, Children)

type Recipe = Map Name (Maybe Node, NodeInfo) -- (Maybe Parent, info)

buildIndex :: [Link] -> Recipe
buildIndex [] = M.empty
buildIndex ls = go M.empty ls
  where
    go :: Recipe -> [Link] -> Recipe
    go m [] = m
    go m (Link name w children : rest) =
      let m' = foldr (\child acc ->
                        M.insertWith
                          (\(parent, _) (_, oldinfo) -> (parent, oldinfo))
                          child
                          (Just (Node name w), (Nothing, []))
                          acc) m children
          m'' = M.insertWith
                  (\(nparent, (nself, nchildren)) (oparent, (omself, ochildren)) -> (oparent, (nself, nchildren)))
                  name
                  (Nothing, (Just (Node name w), children))
                  m'
      in go m'' rest

data Tree = Tree Node [Node] deriving (Eq, Show)

findRoot :: Recipe -> Name
findRoot recipe = fst . head . M.toList $ M.filter (isNothing . fst) recipe

-- buildTree :: Recipe -> Tree
-- buildTree recipe =
--   let root = findRoot recipe
--   in Tree root (go recipe root)
--   where
--     go :: Recipe -> Name -> Tree
--     go = undefined

main :: IO ()
main = do
  contents <- TIO.readFile "input.txt"
  let Right parsed = parseOnly (many1 (parseLink <* endOfLine)) contents
      ind = buildIndex parsed
      parentless = findRoot ind
  print parentless
  pure ()
