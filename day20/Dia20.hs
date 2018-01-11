{-# LANGUAGE ViewPatterns #-}

module Dia20 where

import Data.List (stripPrefix, minimumBy, nubBy)
import Data.List.Split (splitOn)
import Data.Function (on)
import Numeric.LinearAlgebra ((><), rank, Matrix(..))

type Point = (Int, Int, Int)

data Particle = Particle { ind :: Int
                         , pos :: Point
                         , vel :: Point
                         , ace :: Point
                         }
                deriving (Eq, Show)

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

collisionTime1D :: (Eq a, Ord a, Num a, Floating a) => a -> a -> a -> a -> a -> a -> Maybe a
collisionTime1D p0 v0 a0 p1 v1 a1 =
  let
    ps = p0 - p1
    vs = v0 - v1
    as = a0 - a1
    plus = ((-vs + sqrt (vs*vs - 4 * as/2 * ps))/as)
    minus = ((-vs - sqrt (vs*vs - 4 * as/2 * ps))/as)
    only_vs =
      case ((- ps) / vs) >= 0 of
        True -> Just ((- ps) / vs)
        False -> Nothing
    plus_minus = (filter (>= 0) [plus, minus])
    time =
      case length plus_minus of
        0 -> Nothing
        _ -> Just (head plus_minus)
  in
    if as == 0
    then if vs == 0
         then if ps == 0
              then Just 0
              else Nothing
         else only_vs
    else time

tolerance = 1e-3

hasCollision :: Particle -> Particle -> Bool
hasCollision (Particle _ (sx0', sy0', sz0') (vx0', vy0', vz0') (ax0', ay0', az0')) (Particle _ (sx1', sy1', sz1') (vx1', vy1', vz1') (ax1', ay1', az1')) =
  let
    [sx0, sy0, sz0, vx0, vy0, vz0, ax0, ay0, az0, sx1, sy1, sz1, vx1, vy1, vz1, ax1, ay1, az1] = fmap fromIntegral [sx0', sy0', sz0', vx0', vy0', vz0', ax0', ay0', az0', sx1', sy1', sz1', vx1', vy1', vz1', ax1', ay1', az1']
    tx = collisionTime1D sx0 vx0 ax0 sx1 vx1 ax1
    ty = collisionTime1D sy0 vy0 ay0 sy1 vy1 ay1
    tz = collisionTime1D sz0 vz0 az0 sz1 vz1 az1
    not_frac_tx = case tx of
      Just tx -> abs ((fromIntegral . truncate $ tx) - tx) <= tolerance
      Nothing -> False
    not_frac_ty = case ty of
      Just ty -> abs ((fromIntegral . truncate $ ty) - ty) <= tolerance
      Nothing -> False
    not_frac_tz = case tz of
      Just tz -> abs ((fromIntegral . truncate $ tz) - tz) <= tolerance
      Nothing -> False
  in
    all id [ tx == ty
           , ty == tz
           -- , not_frac_tx
           -- , not_frac_ty
           -- , not_frac_tz
           ]

main :: IO ()
main = do
  contents <- lines <$> readFile "input.txt"
  let Just particles = traverse (uncurry parseLine) (zip [0..] contents)
      answer1 = minimumBy (compare `on` modulus2 . ace) particles
      numPs = length particles
      matrix = ((numPs >< 9) (fmap fromIntegral $ concat $ fmap pToList particles)) :: Matrix Double
      survivors = nubBy hasCollision particles
  print answer1
  -- print (rank matrix)
  print (length survivors)
  pure ()
