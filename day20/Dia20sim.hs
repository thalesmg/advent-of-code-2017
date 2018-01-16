module Dia20sim where

import Dia20 ( parseLine
             , positionAt'
             , cmpFPoint
             , Particle(..)
             )

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (groupBy)
import Data.Function (on)

import Debug.Trace

updateParticle :: Particle -> Particle
updateParticle (Particle i (sx', sy', sz') (vx', vy', vz') (ax', ay', az')) =
  let
    [sx, sy, sz, vx, vy, vz, ax, ay, az] = fmap fromIntegral [sx', sy', sz', vx', vy', vz', ax', ay', az']
    vx'' = vx + ax
    vy'' = vy + ay
    vz'' = vz + az
    sx'' = sx + vx''
    sy'' = sy + vy''
    sz'' = sz + vz''
  in
    Particle i (sx'', sy'', sz'') (vx'', vy'', vz'') (ax', ay', az')

stabilization_limit = 1000

simulate :: [Particle] -> Int -> [Particle]
simulate ps last_collision
  | last_collision == stabilization_limit = ps
  | otherwise =
    let
      grouped = groupBy ((==) `on` pos) ps
      ps' = fmap updateParticle $ concat $ filter ((<= 1) . length) $ grouped
      had_collision = length ps' /= length ps
      last_collision' = if had_collision then 0 else last_collision + 1
    in
      simulate ps' last_collision'

main :: IO ()
main = do
  contents <- lines <$> readFile "input.txt"
  let Just particles = traverse (uncurry parseLine) (zip [0..] contents)
      survivors = simulate particles 0
  print (length survivors)
  pure ()
