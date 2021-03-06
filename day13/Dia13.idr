module Dia13

import Data.Fin

%default total

data Sense = Up | Down

data Layer : Type where
  MkLayer : {auto prf : range = S x} ->
            (range : Nat) ->
            (depth : Nat) ->
            (current : Fin (S x)) ->
            (sense : Sense) ->
            Layer

Firewall : Type
Firewall = List Layer

%name Firewall fw

record State where
  constructor MkState
  packet_pos : Nat
  firewall : Firewall
  tick : Nat
  severity : Nat

stepFirewall : Firewall -> Firewall
stepFirewall fw = map mkstep fw
  where
    mkstep : Layer -> Layer
    mkstep (MkLayer (S range) depth current sense) =
      case finToNat current of
        Z => MkLayer (S range) depth (restrict (S range) 1) Down
        S x =>
          if S x == range
            then MkLayer (S range) depth (restrict (S range) (cast x)) Up
            else case sense of
                   Up => MkLayer (S range) depth (restrict (S range) (cast x)) sense
                   Down => MkLayer (S range) depth (restrict (S range) (cast (S (S x)))) sense
    mkstep layer = layer

parseDepth : String -> Maybe Nat
parseDepth str = go (unpack str) []
  where
    go : List Char -> List Char -> Maybe Nat
    go [] _ = Nothing
    go [':'] acc = Just (cast (pack (reverse acc)))
    go (c :: cs) acc = go cs (c :: acc)

parseLayer : String -> Maybe Layer
parseLayer input =
  case words input of
    [depth', range'] =>
      case parseDepth depth' of
           Just depth =>
             let range = cast range'
             in Just (MkLayer range depth (restrict range 0) Down)
           Nothing => Nothing
    _ => Nothing

filterJust : List (Maybe a) -> List a
filterJust [] = []
filterJust (Just x :: xs) = x :: filterJust xs
filterJust (_ :: xs) = filterJust xs

findMaxDepth : Firewall -> Nat
findMaxDepth fw = go fw Z
  where
    go : Firewall -> Nat -> Nat
    go [] acc = acc
    go ((MkLayer range depth current sense) :: xs) acc = go xs (max acc depth)

stepSeverity : Firewall -> (pos : Nat) -> Nat -> Nat
stepSeverity fw pos sev =
  case find (\(MkLayer _ d _ _) => d == pos) fw of
    Nothing => sev
    (Just (MkLayer range depth current sense)) =>
      if finToNat current == Z
        then sev + depth * range
        else sev

stepState : Nat -> State -> Either State State
stepState max_depth state@(MkState pos fw tick sev) =
  if pos > max_depth
    then Left state
    else let sev' = stepSeverity fw pos sev
             fw' = stepFirewall fw
         in Right (MkState (S pos) fw' (S tick) sev')

iteratePacket : (max_depth : Nat) -> State -> State
iteratePacket max_depth state =
  case stepState max_depth state of
    (Left state) => state
    (Right state') => assert_total (iteratePacket max_depth state')

applyNTimes : Nat -> (a -> a) -> a -> a
applyNTimes Z     f a = a
applyNTimes (S k) f a = applyNTimes k f (f a)

partial
iteratePacket' : (max_depth : Nat) -> State -> Nat -> Nat
iteratePacket' max_depth state delay =
  let fw' = applyNTimes delay stepFirewall (firewall state)
  in case severity (iteratePacket max_depth (record {firewall = fw'} state)) of
       Z => delay
       _ => iteratePacket' max_depth state (S delay)

namespace Parte2
  Layer' : Type
  Layer' = (Integer, Integer -> Integer)

  partial
  position' : (range : Integer) -> (tick : Integer) -> Integer
  position' r tick =
    abs (mod (r + tick) (2*r - 1) - r)

  partial
  position : (range : Integer) -> (tick : Integer) -> Integer
  position r tick =
    abs (mod (r - 1 + tick) (2*r - 2) - r + 1)

  parseDepth' : String -> Maybe Integer
  parseDepth' str = go (unpack str) []
    where
      go : List Char -> List Char -> Maybe Integer
      go [] _ = Nothing
      go [':'] acc = Just (cast (pack (reverse acc)))
      go (c :: cs) acc = go cs (c :: acc)

  partial
  parseLayer' : String -> Maybe Layer'
  parseLayer' input =
    case words input of
      [depth', range'] =>
        case parseDepth' depth' of
          Just depth => let range = cast range'
                        in Just (depth, position range)
          _ => Nothing
      _ => Nothing

  partial
  tryDelay : List Layer' -> Integer -> Bool
  tryDelay fw delay = go fw 0 delay
    where
      partial
      go : List Layer' -> Integer -> Integer -> Bool
      go [] p t = True
      go (l@(depth, pos) :: ls) packet_pos tick =
        if depth == packet_pos
          then if pos tick == 0
                  then False
                  else go ls (packet_pos + 1) (tick + 1)
          else go (l :: ls) (packet_pos + 1) (tick + 1)

  partial
  findDelay : List Layer' -> Integer
  findDelay fw = go fw 0 0 0
    where
      partial
      go : List Layer' -> (delay : Integer) -> (packet_pos : Integer) -> (tick : Integer) -> Integer
      go [] delay packet_pos tick = delay
      go ((depth, pos) :: ls) delay packet_pos tick =
        if depth == packet_pos
          then if pos tick == packet_pos
                  then go fw (delay + 1) 0 0
                  else go ls delay (packet_pos + 1) (tick + 1)
          else go ls delay (packet_pos + 1) (tick + 1)

  forM_ : List a -> (a -> IO ()) -> IO ()
  forM_ [] f = pure ()
  forM_ (x :: xs) f = do
    f x
    forM_ xs f

  partial
  teste : List Layer'
  teste = [
    (0, position 3),
    (1, position 2),
    (4, position 4),
    (6, position 4)
  ]

  partial
  tryFindDelay : List Layer' -> List Integer -> IO ()
  tryFindDelay fw [] = pure ()
  tryFindDelay fw (delay :: xs) = do--go x
    print "aaaaaaaaaaaa"
    if (mod delay 1000 == 0)
      then printLn ("Testando " ++ show delay)
      else pure ()
    if tryDelay fw delay
      then printLn ("Achou! " ++ show delay)
      else tryFindDelay fw xs

  partial
  try2 : List Layer' -> Integer -> IO ()
  try2 fw delay = do
    if (mod delay 1000 == 0)
      then printLn ("Testando " ++ show delay)
      else pure ()
    if tryDelay fw delay
      then do
       printLn $ "achooooooo " ++ show delay
       pure ()
      else try2 fw (delay + 1)

  partial
  main : IO ()
  main = do
    Right contents <- readFile "input.txt"
      | Left err => printLn err
    let layers' = traverse id . map parseLayer' $ (lines contents)
    case layers' of
      Nothing => printLn "Bum!"
      (Just fw) => do
        -- let delay = findDelay fw
        -- printLn ("Delay: " ++ show delay)
        printLn "Começando..."
        -- forM_ [0..10] $ \x => do
        --   printLn x
        -- forM_ tries $ \delay => do
        --   printLn delay
        --   when (mod delay 1000 == 0) (printLn (show delay))
        --   when (tryDelay fw delay) (printLn ("Achou! " ++ show delay))
        try2 fw 0
        pure ()

namespace Parte1
  partial
  main : IO ()
  main = do
    Right contents <- readFile "input.txt"
      | Left err => printLn err
    -- let layers = filterJust . map parseLayer $ (lines contents)
    let layers' = traverse id . map parseLayer $ (lines contents)
    case layers' of
      Nothing => printLn "Bum!"
      Just firewall => do
        let state = MkState Z firewall Z Z
        let max_depth = findMaxDepth firewall
        let (MkState _ _ _ sev) = iteratePacket max_depth state
        let delay = iteratePacket' max_depth state Z
        printLn sev
        printLn delay
        pure ()

namespace Main
  partial
  main : IO ()
  main = Parte2.main
