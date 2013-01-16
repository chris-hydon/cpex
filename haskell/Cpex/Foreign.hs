{-# LANGUAGE ForeignFunctionInterface #-}
module Cpex.Foreign () where

import Control.Monad.Trans
import Cpex.Transitions
import CSPM
import CSPM.Foreign
import CSPM.Prelude
import Data.Hashable
import Data.IORef
import Data.Maybe
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Util.PrettyPrint
import qualified Util.MonadicPrettyPrint as M

import qualified Data.Foldable as F
import qualified Data.Sequence as S

type ProcPtr = StablePtr (IORef UProc)
type EventPtr = StablePtr (IORef Event)

-- Foreign exports: Each Ptr argument (not StablePtr) is intended for output.
foreign export ccall
  cpex_transitions :: ProcPtr -> Ptr (Ptr EventPtr) -> Ptr (Ptr ProcPtr) ->
    Ptr CUInt -> IO ()
foreign export ccall
  cpex_event_string :: EventPtr -> Ptr CWString -> Ptr CUChar -> IO ()
foreign export ccall
  cpex_process_string :: SessionPtr -> ProcPtr -> Ptr CWString -> IO CUInt
foreign export ccall
  cpex_process_equal :: ProcPtr -> ProcPtr -> IO Bool
foreign export ccall
  cpex_process_hash :: ProcPtr -> IO CUInt
foreign export ccall
  cpex_process_operator :: ProcPtr -> Ptr CUChar -> IO ()
foreign export ccall
  cpex_op_event :: ProcPtr -> Ptr EventPtr -> IO CUInt
foreign export ccall
  cpex_op_events :: ProcPtr -> Ptr (Ptr EventPtr) -> Ptr CUInt -> IO CUInt
foreign export ccall
  cpex_op_event_map :: ProcPtr -> Ptr (Ptr EventPtr) -> Ptr (Ptr EventPtr) ->
    Ptr CUInt -> IO CUInt
foreign export ccall
  cpex_op_alphabets :: ProcPtr -> Ptr (Ptr (Ptr EventPtr)) ->
    Ptr (Ptr CUInt) -> IO CUInt
foreign export ccall
  cpex_op_process :: ProcPtr -> Ptr ProcPtr -> IO CUInt
foreign export ccall
  cpex_op_process2 :: ProcPtr -> Ptr ProcPtr -> Ptr ProcPtr -> IO CUInt
foreign export ccall
  cpex_op_processes :: ProcPtr -> Ptr (Ptr ProcPtr) -> Ptr CUInt -> IO CUInt
foreign export ccall
  cpex_op_proccall :: ProcPtr -> Ptr ProcPtr -> Ptr CWString -> IO CUInt

foreign export ccall
  cpex_proccall_names :: SessionPtr -> Ptr (Ptr CWString) -> Ptr (Ptr CUInt) ->
    Ptr CUInt -> IO CUInt
foreign export ccall
  cpex_expression_value :: SessionPtr -> CWString -> Ptr ProcPtr -> IO CUInt

-- Builtin process STOP.
builtInName s = name . head . filter ((== s) . stringName) $ builtins False
csp_stop_id = procName (scopeId (builtInName "STOP") [] Nothing)
csp_stop = PProcCall csp_stop_id (POp PExternalChoice S.empty)

-- Given a transition, replaces the process with STOP if the event is Tick.
simplify :: (Event, UProc) -> (Event, UProc)
simplify (Tick, _) = (Tick, csp_stop)
simplify x = x

-- Given a process, returns a numeric value representing the operator.
operatorNum :: UProc -> Int
operatorNum (POp (PAlphaParallel _) _) = 0
operatorNum (PBinaryOp (PException _) _ _) = 1
operatorNum (POp PExternalChoice _) = 2
operatorNum (POp (PGenParallel _) _) = 3
operatorNum (PUnaryOp (PHide _) _) = 4
operatorNum (POp PInternalChoice _) = 5
operatorNum (POp PInterleave _) = 7
operatorNum (PBinaryOp PInterrupt _ _) = 6
operatorNum (PBinaryOp (PLinkParallel _) _ _) = 8
operatorNum (PUnaryOp (POperator _) _) = 9
operatorNum (PUnaryOp (PPrefix _) _) = 10
operatorNum (PUnaryOp (PRename _) _) = 11
operatorNum (PBinaryOp PSequentialComp _ _) = 12
operatorNum (PBinaryOp PSlidingChoice _ _) = 13
operatorNum (PProcCall _ _) = 14

-- Helper functions for reading from and writing to stable pointers.
input :: StablePtr (IORef a) -> IO a
input i = deRefStablePtr i >>= readIORef

output :: a -> Ptr (StablePtr (IORef a)) -> IO ()
output x o = newIORef x >>= newStablePtr >>= poke o

outputArray :: [a] -> Ptr (Ptr (StablePtr (IORef a))) -> IO ()
outputArray xs o = mapM newIORef xs >>= mapM newStablePtr >>= newArray >>= poke o

-- Input: Stable pointer to a process.
-- Output: Array of events offered by the process (may include duplicates).
--         Array of processes resulting from applying the respective event.
--         Size of array.
cpex_transitions :: ProcPtr -> Ptr (Ptr EventPtr) -> Ptr (Ptr ProcPtr) ->
  Ptr CUInt -> IO ()
cpex_transitions inProc outEvents outProcs outCount = do
  p <- input inProc
  let (es, pns) = unzip $ map simplify $ transitions p
  outputArray es outEvents
  outputArray pns outProcs
  liftIO $ poke outCount $ fromIntegral $ length es

-- Input: Event.
-- Output: A string representation of that event, suitable for output.
--         The type of the event (user defined, Tau or Tick).
cpex_event_string :: EventPtr -> Ptr CWString -> Ptr CUChar -> IO ()
cpex_event_string inEvent outName outType = do
  e <- input inEvent
  name <- (newCWString . show . prettyPrint) e
  poke outType $ fromIntegral $ etype e
  poke outName name
  where etype Tau = 1
        etype Tick = 2
        etype _ = 0

-- Input: Process.
-- Output: A string representation of that process, suitable for output.
cpex_process_string :: SessionPtr -> ProcPtr -> Ptr CWString -> IO CUInt
cpex_process_string sessPtr inProc outString = runSession sessPtr $ do
  doc <- liftIO (input inProc) >>= M.prettyPrintBrief
  liftIO $ (newCWString . show) doc >>= poke outString

-- Input: Two processes.
-- Returns: True if the input processes are equal, false if not.
cpex_process_equal :: ProcPtr -> ProcPtr -> IO Bool
cpex_process_equal inProc1 inProc2 = do
  p1 <- input inProc1
  p2 <- input inProc2
  return (p1 == p2)

-- Input: A process.
-- Returns: A hash of that process.
cpex_process_hash :: ProcPtr -> IO CUInt
cpex_process_hash inProc = do
  p <- input inProc
  return $ fromIntegral $ hash p

-- Input: Process.
-- Output: A number representing the operator used.
cpex_process_operator :: ProcPtr -> Ptr CUChar -> IO ()
cpex_process_operator inProc outOp = do
  p <- input inProc
  poke outOp $ fromIntegral $ operatorNum p

-- Input: Process.
-- Output: The single event parameter for the process. Only valid for Prefix
--         processes (a -> P).
-- Error: If the process is of the wrong type.
cpex_op_event :: ProcPtr -> Ptr EventPtr -> IO CUInt
cpex_op_event inProc outEvent = do
  p <- input inProc
  let e = get_event p
  case e of
    Just ev -> liftIO $ do
      output ev outEvent
      return 1
    Nothing -> return 0
  where get_event (PUnaryOp (PPrefix ev) _) = Just ev
        get_event _ = Nothing

-- Input: Process.
-- Output: The set of events parameter for the process, and the number of events
--         in the set. Only valid for Exception (P [|A|> Q), Generalized
--         Parallel (P [|A|] Q) and Hide (P \ A).
-- Error: If the process is of the wrong type.
cpex_op_events :: ProcPtr -> Ptr (Ptr EventPtr) -> Ptr CUInt -> IO CUInt
cpex_op_events inProc outEvents outSize = do
  p <- input inProc
  let es = get_events p
  case es of
    Just es -> liftIO $ do
      outputArray (F.toList es) outEvents
      poke outSize $ fromIntegral $ S.length es
      return 1
    Nothing -> return 0
  where get_events (PBinaryOp (PException evs) _ _) = Just evs
        get_events (POp (PGenParallel evs) _) = Just evs
        get_events (PUnaryOp (PHide evs) _) = Just evs
        get_events _ = Nothing

-- Input: Process.
-- Output: Two lists of events, representing pairs (e1, e2) of events in the
--         event map, and the number of such pairs. Only valid for Linked
--         Parallel (P [A <- B] Q) and Rename (P [[A <- B]]).
-- Error: If the process is of the wrong type.
cpex_op_event_map :: ProcPtr -> Ptr (Ptr EventPtr) -> Ptr (Ptr EventPtr) ->
  Ptr CUInt -> IO CUInt
cpex_op_event_map inProc outLeft outRight outSize = do
  p <- input inProc
  let em = get_event_map p
  case em of
    Just em -> liftIO $ do
      let eml = F.toList em
      outputArray (map fst eml) outLeft
      outputArray (map snd eml) outRight
      poke outSize $ fromIntegral $ S.length em
      return 1
    Nothing -> return 0
  where get_event_map (PBinaryOp (PLinkParallel evm) _ _) = Just evm
        get_event_map (PUnaryOp (PRename evm) _) = Just evm
        get_event_map _ = Nothing

-- Input: Process.
-- Output: A list of lists of events, and a list of sizes for each such list.
--         Only valid for Alphabetized Parallel (P [A||B] Q). The size of each
--         list is equivalent to the number of events in the corresponding
--         process, and the number of lists equal to the number of processes.
-- Error: If the process is of the wrong type.
cpex_op_alphabets :: ProcPtr -> Ptr (Ptr (Ptr EventPtr)) -> Ptr (Ptr CUInt) ->
  IO CUInt
cpex_op_alphabets inProc outAlphas outSizes = do
  p <- deRefStablePtr inProc >>= readIORef
  let as = get_alphas p
  case as of
    Just as -> liftIO $ do
      mapM ((mapM newIORef) . F.toList) (F.toList as)
        >>= mapM (mapM newStablePtr)
        >>= mapM newArray
        >>= newArray
        >>= poke outAlphas
      sizesPtr <- newArray $ (map (fromIntegral . S.length) . F.toList) as
      poke outSizes sizesPtr
      return 1
    Nothing -> return 0
  where get_alphas (POp (PAlphaParallel as) _) = Just as
        get_alphas _ = Nothing

-- Input: PUnaryOp process.
-- Output: The process contained within the operator.
-- Error: If the process is not PUnaryOp.
cpex_op_process :: ProcPtr -> Ptr ProcPtr -> IO CUInt
cpex_op_process inProc outProc = do
  p <- input inProc
  let pn = next p
  case pn of
    Just pn -> liftIO $ do
      output pn outProc
      return 1
    Nothing -> return 0
  where next (PUnaryOp _ pn) = Just pn
        next _ = Nothing

-- Input: PBinaryOp process.
-- Output: The two processes contained within the operator.
-- Error: If the process is not PBinaryOp.
cpex_op_process2 :: ProcPtr -> Ptr ProcPtr -> Ptr ProcPtr -> IO CUInt
cpex_op_process2 inProc outLeft outRight = do
  p <- input inProc
  let pn = next p
  case pn of
    Just (p1, p2) -> liftIO $ do
      output p1 outLeft
      output p2 outRight
      return 1
    Nothing -> return 0
  where next (PBinaryOp _ p1 p2) = Just (p1, p2)
        next _ = Nothing

-- Input: POp process.
-- Output: List of processes contained within the operator.
--         Size of the list.
-- Error: If the process is not POp.
cpex_op_processes :: ProcPtr -> Ptr (Ptr ProcPtr) -> Ptr CUInt -> IO CUInt
cpex_op_processes inProc outProcs outSize = do
  p <- input inProc
  let pn = next p
  case pn of
    Just pn -> liftIO $ do
      outputArray (F.toList pn) outProcs
      poke outSize $ fromIntegral $ S.length pn
      return 1
    Nothing -> return 0
  where next (POp _ pn) = Just pn
        next _ = Nothing

-- Input: PProcCall process.
-- Output: The expansion of the process.
--         The string representation of the process.
-- Error: If the process is not PProcCall.
cpex_op_proccall :: ProcPtr -> Ptr ProcPtr -> Ptr CWString -> IO CUInt
cpex_op_proccall inProc outProc outName = do
  p <- input inProc
  let pn = next p
  case pn of
    Just (n, pn) -> liftIO $ do
      output pn outProc
      name <- (newCWString . show . prettyPrint) n
      poke outName name
      return 1
    Nothing -> return 0
  where next (PProcCall n pn) = Just (n, pn)
        next _ = Nothing

-- Input: CSPM session.
-- Output: Array of strings representing the bound names of proc calls.
--         Array of unsigned ints representing the number of parameters for the
--         respective proc call.
cpex_proccall_names :: SessionPtr -> Ptr (Ptr CWString) -> Ptr (Ptr CUInt) ->
  Ptr CUInt -> IO CUInt
cpex_proccall_names sessPtr outNames outParams outCount = runSession sessPtr $ do
  ns <- getBoundNames
  let customs = filter ((flip notElem) $ map name $ builtins False) ns
  pairs <- mapM maybePair customs
  let (names, paramCounts) = unzip $ catMaybes pairs
  namesList <- liftIO $ mapM (newCWString . show) names
  namesArray <- liftIO $ newArray namesList
  paramsArray <- liftIO $ newArray $ map fromIntegral paramCounts
  liftIO $ poke outNames namesArray
  liftIO $ poke outParams paramsArray
  liftIO $ poke outCount $ fromIntegral $ length names
  where maybePair name = do
          ForAll _ t <- typeOfName name
          return (isProcCall (name, 0) t)
        isProcCall (name, x) TProc = Just (name, x)
        isProcCall (name, x) (TFunction _ p) = isProcCall (name, x + 1) p
        isProcCall _ _ = Nothing

-- Input: CSPM session.
--        CSPM expression string.
-- Output: The process represented by that expression.
-- Error: If the given expression does not resolve to a process.
cpex_expression_value :: SessionPtr -> CWString -> Ptr ProcPtr -> IO CUInt
cpex_expression_value sessPtr inName outProc = runSession sessPtr $ do
  name <- liftIO $ peekCWString inName
  VProc expressionValue <- stringToValue TProc name
  liftIO $ output expressionValue outProc
