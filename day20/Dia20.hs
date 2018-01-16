{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Dia20 where

import Data.List (stripPrefix, minimumBy, nubBy, sortOn)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Aeson
import Data.Function (on)
import Data.Maybe (catMaybes, maybeToList, listToMaybe, isJust)
import Control.Monad (join)
import Numeric.LinearAlgebra ((><), rank, Matrix(..))

import Debug.Trace

type Point = (Int, Int, Int)
type FPoint = (Double, Double, Double)

data Particle = Particle { ind :: Int
                         , pos :: Point
                         , vel :: Point
                         , ace :: Point
                         }
                deriving (Eq, Show, Ord)

parsePoint :: String -> Maybe Point
parsePoint (stripPrefix "=<" . tail -> Just (stripPrefix ">" . reverse -> Just (splitOn "," . reverse -> [x, y, z]))) = Just (read x, read y, read z)
parsePoint _ = Nothing

parseLine :: Int -> String -> Maybe Particle
parseLine i (splitOn ", " -> fmap parsePoint -> [Just p, Just v, Just a]) = Just (Particle i p v a)
parseLine _ _ = Nothing

modulus2 :: Point -> Int
modulus2 (x, y, z) = x*x + y*y + z*z

pToList :: Particle -> [Int]
pToList (Particle _ (x, y, z) (vx, vy, vz) (ax, ay, az)) = [x,y,z,vx,vy,vz,ax,ay,az]

collisionTime1D' :: (Eq a, Ord a, Num a, Floating a, RealFloat a) => a -> a -> a -> a -> a -> a -> Maybe [a]
collisionTime1D' p0 v0 a0 p1 v1 a1 =
  let
    ps = p0 - p1
    vs = v0 - v1
    as = a0 - a1
    a = as
    b = 2*vs - as
    c = 2*ps
    plus  = ((-b + sqrt (b*b - 4 * a * c))/2/a)
    minus = ((-b - sqrt (b*b - 4 * a * c))/2/a)
    only_vs =
      case ((- c) / b) >= 0 of
        True -> Just [(- c) / b]
        False -> Nothing
    -- plus_minus = filter (\t -> (not . isNaN) t && t >= 0) [plus, minus]
    plus_minus = filter (>= 0) [plus, minus]
    time =
      case length plus_minus of
        0 -> Nothing
        _ -> Just plus_minus
  in
    if as == 0
    then if vs == 0
         then if ps == 0
              then Just [0]
              else Nothing
         else only_vs
    else time

collisionTime1D :: (Eq a, Ord a, Num a, Floating a, RealFloat a) => a -> a -> a -> a -> a -> a -> Maybe [a]
collisionTime1D p0 v0 a0 p1 v1 a1 =
  let
    ps = p0 - p1
    vs = v0 - v1
    as = a0 - a1
    plus = ((-vs + sqrt (vs*vs - 2 * as * ps))/as)
    minus = ((-vs - sqrt (vs*vs - 2 * as * ps))/as)
    only_vs =
      case ((- ps) / vs) >= 0 of
        True -> Just [(- ps) / vs]
        False -> Nothing
    plus_minus = filter (\t -> (not . isNaN) t && t >= 0) [plus, minus]
    time =
      case length plus_minus of
        0 -> Nothing
        _ -> Just plus_minus
  in
    if as == 0
    then if vs == 0
         then if ps == 0
              then Just [0]
              else Nothing
         else only_vs
    else time

positionAt :: Particle -> Double -> FPoint
positionAt (Particle _ (sx, sy, sz) (vx, vy, vz) (ax, ay, az)) t =
  ( fromIntegral sx + fromIntegral vx * t + fromIntegral ax / 2 *t*t
  , fromIntegral sy + fromIntegral vy * t + fromIntegral ay / 2 *t*t
  , fromIntegral sz + fromIntegral vz * t + fromIntegral az / 2 *t*t
  )

positionAt' :: Particle -> Double -> FPoint
positionAt' (Particle _ (sx', sy', sz') (vx', vy', vz') (ax', ay', az')) t =
  let
    [sx, sy, sz, vx, vy, vz, ax, ay, az] = fmap fromIntegral [sx', sy', sz', vx', vy', vz', ax', ay', az']
  in
    ( sx + vx * t + ax / 2 * t * (t + 1)
    , sy + vy * t + ay / 2 * t * (t + 1)
    , sz + vz * t + az / 2 * t * (t + 1)
    )

cmpFPoint :: FPoint -> FPoint -> Bool
cmpFPoint (x0, y0, z0) (x1, y1, z1) =
  all (<= tolerance) [ abs (x1 - x0)
                     , abs (y1 - y0)
                     , abs (z1 - z0)
                     ]

collidesIn :: Particle -> Particle -> [Double] -> Bool
collidesIn particle_a particle_b ts =
  let
    poss_a = fmap (positionAt' particle_a) ts
    poss_b = fmap (positionAt' particle_b) ts
  in
    any id $ do
               pos_a <- poss_a
               pos_b <- poss_b
               pure $ pos_a == pos_b
               pure $ cmpFPoint pos_a pos_b

tolerance = 1e-1

isInteger x = abs ((fromIntegral . truncate $  x) - x) <= tolerance

hasCollision :: Particle -> Particle -> Bool
hasCollision particle_a@(Particle _ (sx0', sy0', sz0') (vx0', vy0', vz0') (ax0', ay0', az0')) particle_b@(Particle _ (sx1', sy1', sz1') (vx1', vy1', vz1') (ax1', ay1', az1')) =
  let
    [sx0, sy0, sz0, vx0, vy0, vz0, ax0, ay0, az0, sx1, sy1, sz1, vx1, vy1, vz1, ax1, ay1, az1] = fmap fromIntegral [sx0', sy0', sz0', vx0', vy0', vz0', ax0', ay0', az0', sx1', sy1', sz1', vx1', vy1', vz1', ax1', ay1', az1']
    mtxs = collisionTime1D' sx0 vx0 ax0 sx1 vx1 ax1
    mtys = collisionTime1D' sy0 vy0 ay0 sy1 vy1 ay1
    mtzs = collisionTime1D' sz0 vz0 az0 sz1 vz1 az1
    txs = join $ maybeToList mtxs
    tys = join $ maybeToList mtys
    tzs = join $ maybeToList mtzs
    ts = txs ++ tys ++ tzs
    all_coords_have_ts = all isJust [mtxs, mtys, mtzs]
    all_ts_integer = all isInteger ts
    -- not_frac_tx = case tx of
    --   Just tx -> abs ((fromIntegral . truncate $ tx) - tx) <= tolerance
    --   Nothing -> False
    -- not_frac_ty = case ty of
    --   Just ty -> abs ((fromIntegral . truncate $ ty) - ty) <= tolerance
    --   Nothing -> False
    -- not_frac_tz = case tz of
    --   Just tz -> abs ((fromIntegral . truncate $ tz) - tz) <= tolerance
    --   Nothing -> False
  in
    all id [ all_coords_have_ts
           -- , all_ts_integer
           , collidesIn particle_a particle_b ts
           ]

collisionTimes :: Particle -> Particle -> [Double]
collisionTimes particle_a@(Particle _ (sx0', sy0', sz0') (vx0', vy0', vz0') (ax0', ay0', az0')) particle_b@(Particle _ (sx1', sy1', sz1') (vx1', vy1', vz1') (ax1', ay1', az1')) =
  let
    [sx0, sy0, sz0, vx0, vy0, vz0, ax0, ay0, az0, sx1, sy1, sz1, vx1, vy1, vz1, ax1, ay1, az1] = fmap fromIntegral [sx0', sy0', sz0', vx0', vy0', vz0', ax0', ay0', az0', sx1', sy1', sz1', vx1', vy1', vz1', ax1', ay1', az1']
    mtxs = collisionTime1D' sx0 vx0 ax0 sx1 vx1 ax1
    mtys = collisionTime1D' sy0 vy0 ay0 sy1 vy1 ay1
    mtzs = collisionTime1D' sz0 vz0 az0 sz1 vz1 az1
    txs = join $ maybeToList mtxs
    tys = join $ maybeToList mtys
    tzs = join $ maybeToList mtzs
  in
    txs ++ tys ++ tzs

leastCollisionTime :: Particle -> Particle -> Maybe Double
leastCollisionTime particle_a particle_b =
  let
    ts = collisionTimes particle_a particle_b
    col_ts = filter (collidesIn particle_a particle_b . (:[])) ts
  in
    listToMaybe col_ts

allCollisions :: [Particle] -> [(Int, [(Double, Particle)])]
allCollisions lst = do
  p <- lst
  let i = ind p
      cols = fmap (\(Just x, p') -> (x, p')) . filter (isJust . fst) . fmap (\p' -> (leastCollisionTime p p', p')) . filter ((==) i . ind) $ lst
  pure (i, cols)

answer2 particles = go particles
  where
    go [] = []
    go (p:ps) =
      let
        cols = filter (p `hasCollision`) ps
        noCols = filter (not . hasCollision p) ps
      in
        if cols == []
        then p : go ps
        else go noCols

answer2' particles = go particles
  where
    go [] = []
    go (p:ps) =
      let
        -- least = join $ fmap (maybeToList . leastCollisionTime p) ps
        cols = foldr (\p' acc -> case leastCollisionTime p p' of
                                   Nothing -> acc
                                   Just t  -> (t, p') : acc) [] ps
        near x y = abs (x - y) <= tolerance
        -- next = case sortOn fst cols of
        --           [] -> Nothing
        --           ((t, _) : _) -> Just $ fmap snd . filter (not . near t . fst) $ cols
        sorted = traceShow cols $ sortOn fst cols
        to_filter_out = case sorted of
                          [] -> []
                          ((t, _) : _) -> ind p : (fmap (ind . snd) . filter (not . near t . fst) $ cols)
        next = filter (\p' -> not $ ind p' `elem` to_filter_out) ps
      in
        case sorted of
          [] -> p : go ps
          _ -> go next

allCols :: S.Set Particle -> M.Map Double (S.Set Int)
allCols ps = foldr (\p acc -> M.union (updateP p) acc) M.empty ps
  where
    updateP p = foldr (\p' acc -> case collisionTimes p p' of
                         [] -> acc
                         ts -> foldr (\t acc' -> M.insertWith S.union t (S.fromList [ind p', ind p]) acc') acc ts)
                   M.empty
                   (S.delete p ps)

answer2'' m ps
  | M.null m  = ps
  | otherwise =
    let
      ((t, inds), m') = M.deleteFindMin m
      m'' = M.filter (\s -> S.size s > 2) $ M.map (\s -> S.difference s inds) $ m'
      ps' = S.difference ps inds
    in
      -- traceShow (encode m'')
        (answer2'' m'' ps')

main :: IO ()
main = do
  contents <- lines <$> readFile "input.txt"
  let Just particles = traverse (uncurry parseLine) (zip [0..] contents)
      answer1 = minimumBy (compare `on` modulus2 . ace) particles
      numPs = length particles
      -- matrix = ((numPs >< 9) (fmap fromIntegral $ concat $ fmap pToList particles)) :: Matrix Double
      -- survivors = nubBy hasCollision particles
      sps = S.fromList particles
      sinds = S.map ind sps
      colsMap = (allCols sps)
      survivors = answer2'' (M.filterWithKey (\t _ -> isInteger t) colsMap) sinds
  print answer1
  -- print (encode colsMap)
  -- print (rank matrix)
  print (M.keys colsMap)
  print (length survivors)
  pure ()
