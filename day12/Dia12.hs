{-# LANGUAGE LambdaCase #-}

module Dia12 where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)
import Data.List.Split (splitOn)
import Data.Maybe (maybe, fromMaybe, mapMaybe)

data Link = Link Int (Set Int)
data UnionFind = UnionFind { parents :: Map Int Int
                           , components :: Map Int (Set Int)
                           }
                 deriving Show

parseLink :: String -> Link
parseLink str =
  let (node' : "<->" : linked') = words str
      node = read node'
      linked = S.insert node
               $ S.fromList
               $ read <$> splitOn "," (concat linked')
  in Link node linked

findRoot :: Map Int Int -> Int -> Maybe Int
findRoot parents node =
  case M.lookup node parents of
    Nothing -> Nothing
    Just parent -> if parent == node
                      then Just parent
                      else findRoot parents parent

findRootPath :: Map Int Int -> Int -> [Int]
findRootPath parents node =
  case M.lookup node parents of
    Nothing -> []
    Just parent -> if parent == node
                      then [parent]
                      else parent : findRootPath parents parent

union :: UnionFind -> Link -> UnionFind
union uf (Link node links) =
  let parent_map = parents uf
      comp_map = components uf
      mparent = findRoot parent_map node
      root = fromMaybe node mparent
      links_parents' =
        foldr (\child acc ->
                 case findRootPath parent_map child of
                   [] -> acc
                   parents -> foldr S.insert acc parents)
              S.empty
              links
      links_parents =
        foldr (\child acc ->
                 case findRoot parent_map child of
                      Just parent -> S.insert parent acc
                      Nothing -> acc)
              S.empty
              links
      links_root =
        if S.null links_parents'
          then root
          else S.findMin links_parents'
      the_root = min links_root root
      parent_map' = foldr (\child acc -> M.insert child the_root acc) parent_map (S.union links links_parents')
      comp_map' =
        let old_roots_comp_map = M.filterWithKey (\k _ -> S.member k links_parents) comp_map
            previous_components = mconcat $ M.elems old_roots_comp_map
            comp_map'' = M.filterWithKey (\k _ -> not $ S.member k links_parents) comp_map
        in M.insertWith S.union the_root (S.union links previous_components) comp_map''
  in UnionFind{parents = parent_map', components = comp_map'}

main :: IO ()
main = do
  contents <- fmap parseLink . lines <$> readFile "input.txt"
  let uf0 = UnionFind M.empty M.empty
      uf = foldl union uf0 contents
  print uf
  print $ maybe 0 S.size (M.lookup 0 (components uf))
  pure ()
