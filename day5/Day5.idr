module Day5

import Data.Vect

namespace Part1
  readVect : String -> IO (n ** Vect n Int)
  readVect path = do
    Right handle <- openFile path Read
      | Left err => do
        print err
        pure (_ ** [])
    go handle
  where
    go : File -> IO (n ** Vect n Int)
    go handle = do
      eof <- fEOF handle
      case eof of
        False => do
          Right x' <- fGetLine handle
            | Left err => do
              print err
              pure (_ ** [])
          if x' == ""
            then pure (_ ** [])
            else do
              let x = the Int (cast x')
              (_ ** xs) <- go handle
              pure (_ ** (x :: xs))
        True => pure (_ ** [])

  countIter : Vect n Int -> (Nat, Vect n Int)
  countIter [] = (0, [])
  countIter lst@(x :: xs) = go 0 0 lst
    where
      go : (index : Int) -> (count : Nat) -> (v : Vect n Int) -> (Nat, Vect n Int)
      go index count {n} v =
        case integerToFin (cast index) n of
          Nothing => (count, v)
          (Just ind) =>
            let index' = Data.Vect.index ind v + index
                v' = updateAt ind (+1) v
            in go index' (S count) v'

  countIter2 : Vect n Int -> (Nat, Vect n Int)
  countIter2 [] = (0, [])
  countIter2 lst@(x :: xs) = go 0 0 lst
    where
      go : (ind : Int) -> (count : Nat) -> (v : Vect n Int) -> (Nat, Vect n Int)
      go ind count {n} v =
        case integerToFin (cast ind) n of
          Nothing => (count, v)
          (Just find) =>
            let jump = index find v
                offset = if jump >= 3 then -1 else 1
                ind' = jump + ind
                v' = updateAt find (+ offset) v
            in go ind' (S count) v'

  main : IO ()
  main = do
    (n ** v) <- readVect "input.txt"
    -- let res = countIter v
    -- print res
    let (res2, _) = countIter2 v
    print res2
    pure ()


namespace Main
  main : IO ()
  main = do
    (n ** v) <- readVect "input.txt"
    -- let res = countIter v
    -- print res
    let (res2, _) = countIter2 v
    print res2
    pure ()
