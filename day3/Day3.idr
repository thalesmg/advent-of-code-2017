module Day3

namespace Part1
  input : Integer
  input = 265149

  radius : Integer -> Integer
  radius num = go num 1
    where
      go : Integer -> Integer -> Integer
      go num x = if num > x * x
                    then go num (x + 2)
                    else div (x - 1) 2

  dist : Integer -> Integer
  dist x = radius x + mod (x - (1 + radius x)) (1 + radius x)

  boom : Integer -> Integer
  boom x = mod (radius x - x) (2 * radius x)

  odist : Integer -> Integer
  odist x = mod (x - (1 + radius x)) (2 * radius x) + boom x

  plof : Integer -> Integer -> Integer
  plof y phase = let r = radius y
                     r' = 2 * r + 1
                 in mod (y + phase - (r' * r')) (2 * r)

  pluf : Integer -> Integer -> Integer
  pluf y phase = let r = radius y
                     r' = 2 * r + 1
                 in mod (- y + phase + (r' * r')) (2 * r)

  kabum : Integer -> Integer -> Integer
  kabum x phase =
    div (abs (plof x (2 * radius x - 1) - pluf x (-1))) 2 + radius x
