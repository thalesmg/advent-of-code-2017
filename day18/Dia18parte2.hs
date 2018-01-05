{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}

module Dia18parte2 where

import Dia18 ( parse1
             , Register(..)
             , Value
             , Program
             , Processor
             , Instruction(..)
             , Zipper
             , get
             , put
             , deref
             )

import Control.Monad (when)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as S
import qualified Data.Map as M
import Debug.Trace

type SentMsgs = Int

type Queue = Seq Int

data Process = Process !Queue !Queue !SentMsgs !State1 deriving Show

data State1 = Running !Processor !Program
            | WaitingForInput !Register !Processor !Program
            | Terminated
            deriving Show

data State = State !Process !Process deriving Show

move' :: Int -> Zipper a -> Maybe (Zipper a)
move' !n prog@(!prev, !inst, !next)
  | n == 0    = Just prog
  | n >  0    =
    case next of
      [] -> Nothing
      (i:next') -> move' (n - 1) (inst : prev, i, next')
  | otherwise =
    case prev of
      [] -> Nothing
      (i:prev') -> move' (n + 1) (prev', i, inst : next)

safeMove :: Int -> Process -> Process
safeMove 0 process = process
safeMove !n (Process !inq !outq !sent !st) =
  case st of
    Running processor prog ->
      case move' n prog of
        Just !prog' -> Process inq outq sent (Running processor prog')
        Nothing -> Process inq outq sent Terminated
    WaitingForInput reg processor prog ->
      case move' n prog of
        Just !prog' -> Process inq outq sent (WaitingForInput reg processor prog')
        Nothing -> Process inq outq sent Terminated
    _ -> Process inq outq sent st

execute1 :: Int -> Process -> Process
execute1 pid (Process !inq !outq !sent (Running !processor prog@(_, !inst, _))) =
  case inst of
    Snd reg ->
      traceShow (show pid ++ ": SND " ++ show reg ++ " -> " ++ show (get processor reg))
      safeMove 1 (Process inq (outq S.|> get processor reg) (sent + 1) (Running processor prog))
    Set reg val ->
      traceShow (show pid ++ ": SET " ++ show reg ++ " , " ++ show (deref processor val))
      safeMove 1 (Process inq outq sent (Running (put processor reg (deref processor val)) prog))
    Add reg val ->
      traceShow (show pid ++ ": ADD " ++ show reg ++ " , " ++ show (deref processor val))
      safeMove 1 (Process inq outq sent (Running (put processor reg (deref processor val + get processor reg)) prog))
    Mul reg val ->
      traceShow (show pid ++ ": MUL " ++ show reg ++ " , " ++ show (deref processor val))
      safeMove 1 (Process inq outq sent (Running (put processor reg (deref processor val * get processor reg)) prog))
    Mod reg val ->
      traceShow (show pid ++ ": MOD " ++ show reg ++ " , " ++ show (deref processor val))
      safeMove 1 (Process inq outq sent (Running (put processor reg (get processor reg `mod` deref processor val)) prog))
    Rcv reg ->
      traceShow (show pid ++ ": RCV " ++ show reg ++ " -> " ++ show (get processor reg)) $
      case inq of
        Empty -> safeMove 1 (Process Empty outq sent (WaitingForInput reg processor prog))
        msg :<| inq' -> safeMove 1 (Process inq' outq sent (Running (put processor reg msg) prog))
    Jgz reg val ->
      let
        reg_val = deref processor reg
      in
        traceShow (show pid ++ ": JGZ " ++ show reg ++ " , " ++ show (deref processor val)) $
        if reg_val > 0 && deref processor val /= 0
        then safeMove (deref processor val) (Process inq outq sent (Running processor prog))
        else safeMove 1 (Process inq outq sent (Running processor prog))
execute1 pid (Process (msg :<| inq) !outq !sent (WaitingForInput !reg !processor !prog)) =
  execute1 pid (Process inq outq sent (Running (put processor reg msg) prog))
execute1 pid !state = state

sendMsgs :: (Process, Process) -> (Process, Process)
sendMsgs (Process !in0 !out0 !sent0 !st0, Process !in1 !out1 !sent1 !st1) =
  (Process (in0 S.>< out1) S.empty sent0 st0, Process (in1 S.>< out0) S.empty sent1 st1)

execute :: State -> SentMsgs
execute (State p0 p1) =
  case (st0, st1) of
    (WaitingForInput{}, WaitingForInput{}) ->
      case (in0, in1) of
        (Empty, Empty) -> sent1
        _ -> execute (State p0' p1')
    (_, Terminated{}) -> sent1
    (Terminated{}, WaitingForInput{}) -> sent1
    (_, _) -> execute (State p0' p1')
  where
    (!p0'@(Process !in0 !out0 !sent0 !st0), !p1'@(Process !in1 !out1 !sent1 !st1)) = sendMsgs (execute1 0 p0, execute1 1 p1)

-- execute2 :: State -> IO (SentMsgs, State)
-- execute2 (State p0 p1@(Process _ _ old _)) = do
--   -- when has_new_msg (print "produced")
--   case (st0, st1) of
--     (WaitingForInput{}, WaitingForInput{}) -> pure (sent1, State p0' p1')
--     (Terminated{}, WaitingForInput{}) -> pure (sent1, State p0' p1')
--     (_, Terminated{}) -> pure (sent1, State p0' p1')
--     (_, _) -> do
--       -- print "running p0"
--       execute2 $ State p0' p1'
--     -- (WaitingForInput{}, Running{}) -> do
--     --   -- print "p0 blocked"
--     --   -- when has_new_msg (print "produced msg")
--     --   execute2 $ State p0' p1'
--     -- (Terminated{}, Running{}) -> do
--     --   -- print "p0 terminated"
--     --   execute2 $ State p0' p1'
--   where
--     (!p0'@(Process !in0 !out0 !sent0 !st0), !p1'@(Process !in1 !out1 !sent1 !st1)) = sendMsgs (execute1 p0, execute1 p1)
--     has_new_msg = sent1 /= old

initialize :: [Instruction] -> Int -> Process
initialize (inst:insts) pid =
  Process S.empty S.empty 0 (Running (M.singleton (Register 'p') pid) ([], inst, insts))

main :: IO ()
main = do
  instructions@(inst : rest) <- fmap parse1 . lines <$> readFile "input.txt"
  let p0 = initialize instructions 0
      p1 = initialize instructions 1
      st = State p0 p1
      sent1 = execute st
  -- (sent2, st2) <- execute2 st
  -- print st'
  print sent1
  -- print sent2
  -- print st2
  pure ()
