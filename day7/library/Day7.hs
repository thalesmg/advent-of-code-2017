{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Day7 where

import Prelude hiding (takeWhile)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Applicative
import Data.Monoid
import Control.Monad (forM_)
import Data.List (sortOn)
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

data Tree a = Tree a [Tree a] deriving (Eq, Show)

instance Functor Tree where
  fmap f (Tree a as) = Tree (f a) ((fmap . fmap) f as)

instance Foldable Tree where
  foldMap f (Tree a as) = f a `mappend` foldMap (foldMap f) as

findRoot :: Recipe -> Name
findRoot recipe = fst . head . M.toList $ M.filter (isNothing . fst) recipe

buildTree :: Recipe -> Tree Node
buildTree recipe =
  let root = findRoot recipe
      (Nothing, (Just node, children)) = recipe M.! root
  in Tree node (fmap go children)
  where
    go :: Name -> Tree Node
    go name = let (_, (Just self, children)) = recipe M.! name
              in Tree self (fmap go children)

weightNode :: Node -> Weight
weightNode (Node _ w) = w

weight :: Tree Node -> Weight
weight = getSum . foldMap (Sum . weightNode)

weightTree :: Tree Node -> Tree (Weight, Weight)
weightTree (Tree (Node _ w) nodes) =
  let wts = fmap weightTree nodes
      ws  = sum (fmap weight nodes)
  in Tree (w, w + ws) wts

treesToLevels :: [Tree a] -> [[a]]
treesToLevels [] = []
treesToLevels trees =
  let level   = fmap (\(Tree w _) -> w) trees
      nextLevel = concatMap (\(Tree _ ts) -> ts) trees
  in  level : treesToLevels nextLevel

-- findProblemLevel :: [Tree (Weight, Weight)] -> [(Weight, Weight)]
-- findProblemLevel [] = []
-- findProblemLevel trees =
--   let levelWs   = fmap (\(Tree w _) -> w) trees
--       numWs     = length levelWs
--       allSame   = numWs * (snd . head $ levelWs) == sum (fmap snd levelWs)
--       nextLevel = concatMap (\(Tree _ ts) -> ts) trees
--   in
--     if allSame
--     then findProblemLevel nextLevel
--     else levelWs

-- findCorrection :: [(Weight, Weight)] -> Weight -- ((Weight, Weight), (Weight, Weight))
-- findCorrection lst =
--   let total_ws = sortOn snd lst
--       x = head total_ws
--       ((problem, abnormalW), (_, normalW)) = findAnomaly snd x x total_ws
--   in normalW - (abnormalW - problem) --  findAnomaly snd x x total_ws
--     where
--       findAnomaly :: (Eq a, Eq b) => (a -> b) -> a -> a -> [a] -> (a, a)
--       findAnomaly _ one two [] = (two, one)
--       findAnomaly f one two (x:xs)
--         | one == two = if f x == f one then findAnomaly f one two xs else findAnomaly f one x xs
--         | otherwise  = if f x == f one then (two, one) else (one, two)

findAnomaly :: (Eq a, Eq b) => (a -> b) -> a -> a -> [a] -> (a, a)
findAnomaly _ one two [] = (two, one)
findAnomaly f one two (x:xs)
  | one == two = if f x == f one then findAnomaly f one two xs else findAnomaly f one x xs
  | otherwise  = if f x == f one then (two, one) else (one, two)

trackAnomalies :: (Eq a, Eq b) => (a -> b) -> Tree a -> [(Tree a, Tree a)]
trackAnomalies _ (Tree _ []) = []
trackAnomalies f tree@(Tree _ (child:rest)) =
  let (anomaly, normal) = findAnomaly (\(Tree w _) -> f w) child child rest
  in (anomaly, normal) : trackAnomalies f anomaly

findProblemLevel :: [Tree (Weight, Weight)] -> (Tree (Weight, Weight), Tree (Weight, Weight))
findProblemLevel [] = (Tree (-1, -1) [], Tree (-1, -1) [])
findProblemLevel (x:xs) =
  let (anomaly@(Tree _ children), normal) = findAnomaly (\(Tree (_, w) _) -> w) x x (x:xs)
      numChildren = length children
      childrenOk =
        if numChildren == 0
        then True
        else let (Tree (_, w1) _ : _) = children
             in numChildren * w1 == sum (fmap (\(Tree (w, _) _) -> w) children)
  in if childrenOk
     then (anomaly, normal)
     else findProblemLevel children

findProblemLevel2 :: [Tree (Weight, Weight)] -> (Tree (Weight, Weight), Tree (Weight, Weight))
findProblemLevel2 [] = (Tree (-1, -1) [], Tree (-1, -1) [])
findProblemLevel2 [Tree _ ts] = findProblemLevel2 ts
findProblemLevel2 (t:ts) =
  let (anomaly@(Tree _ children), normal) = findAnomaly (\(Tree (_, w) _) -> w) t t ts
      childrenOk = let (Tree (_, w1) _, Tree (_, w2) _) = findProblemLevel2 children
                   in w1 == w2
  in if childrenOk
     then (anomaly, normal)
     else findProblemLevel2 [anomaly]


main :: IO ()
main = do
  contents <- TIO.readFile "input.txt"
  let Right parsed = parseOnly (many1 (parseLink <* endOfLine)) contents
      ind = buildIndex parsed
      parentless = findRoot ind
  print parentless
  let tree = buildTree ind
      wtree = weightTree tree
      (Tree (w1, total1) _, Tree (w2, total2) _) = findProblemLevel2 [wtree]
      solution = total2 - (total1 - w1)
      -- plevel = findProblemLevel [wtree]
      -- anomaly = findCorrection plevel
  -- forM_ (treesToLevels [wtree]) print
  -- print (findAnomaly (\(Tree (_, x) _) -> x) wtree wtree [wtree])
  -- print (findProblemLevel [wtree])
  -- forM_ (trackAnomalies snd wtree) $ \(Tree w children, Tree ww cc) -> do
  --   print (w, ww)
  --   forM_ children $ \(Tree w _) -> print $ "  " ++ show w
  -- print plevel
  -- print anomaly
  -- print (findProblemLevel2 [wtree])
  print solution
  pure ()
