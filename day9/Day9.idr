module Day9

%default total

-- data StreamState : Type where
--   Consuming : (chars : List Char) -> (level : Nat) -> (score : Nat) -> StreamState
--   Garbage : (chars : List Char) -> (level : Nat) -> (score : Nat) -> StreamState
--   Done : (level : Nat) -> (score : Nat) -> StreamState

-- data IsRunning : StreamState -> Type where
--   IsConsuming : IsRunning (Consuming chars level score)
--   IsGarbaging : IsRunning (Garbage chars level score)

-- data StreamCmd : Type -> StreamState -> StreamState -> Type where
--   Deepen : StreamCmd () (Consuming level score) (Consuming (S level) score)
--   Shallow : StreamCmd () (Consuming (S level) score) (Consuming level (score + (S level)))
--   IgnoreNext : StreamCmd () (Consuming level score) (Consuming level score)
--   EnterGarbage : StreamCmd () (Consuming level score) (Garbage level score)
--   ExitGarbage : StreamCmd () (Garbage level score) (Consuming level score)
--   Consume : {auto prf : IsRunning state} -> StreamCmd () state state

--   Chain : StreamCmd ty1 state1 state2 ->
--           StreamCmd ty2 res state3 ->
--           StreamCmd ty2 state1 state3

-- run : List Char -> StreamState
-- run [] = Consuming Z Z
-- run (x :: xs) = go (Consuming Z Z) (x :: xs)
--   where
--     go : StreamState -> List Char -> StreamState
--     go state [] = state
--     go state ('!' :: _ :: xs) = go state xs
--     go state@(Garbage level score) (_ :: xs) = go state xs
--     go state@(Garbage level score) (_ :: xs) = go state xs
--     go state ('{' :: xs) =

record State where
  constructor MkState
  level : Nat
  score : Nat
  cleaned : Nat
  garbage : Bool

process : State -> List Char -> State
process state [] = state
process state ('!' :: _ :: xs) = process state xs
process state@(MkState _ _ _ True) ('>' :: xs) =
  process (record {garbage = False} state) xs
process state@(MkState _ _ _ True) (_ :: xs) =
  process (record {cleaned $= (+1)} state) xs
process state@(MkState _ _ _ False) ('}' :: xs) =
  process (record {level $= (\l => case l of
                                        S l => l
                                        Z => Z), score $= (+ (level state))} state) xs
process state ('{' :: xs) = process (record {level $= (+1)} state) xs
process state ('<' :: xs) = process (record {garbage = True} state) xs
process state (_ :: xs) = process state xs

main : IO ()
main = do
  Right contents <- readFile "input.txt"
    | Left err => print err
  let list = unpack contents
  let (MkState _ score cleaned _) = process (MkState Z Z Z False) list
  putStrLn $ show score
  putStrLn $ show cleaned
