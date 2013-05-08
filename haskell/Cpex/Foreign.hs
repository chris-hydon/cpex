{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances, TypeSynonymInstances #-}
module Cpex.Foreign () where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans
import Cpex.Transitions
import CSPM
import CSPM.Foreign
import CSPM.Prelude
import Data.Hashable
import Data.Maybe
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Util.Exception
import Util.PrettyPrint
import qualified Util.MonadicPrettyPrint as M

import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.HashTable.IO as H

type ProcPtr = StablePtr UProc
type EventPtr = StablePtr Event
type HashTable k v = H.BasicHashTable k v

-- Foreign exports: Each Ptr argument (not StablePtr) is intended for output.
foreign export ccall
  cpex_transitions :: SessionPtr -> ProcPtr -> Bool -> Ptr (Ptr EventPtr) ->
    Ptr (Ptr ProcPtr) -> Ptr CUInt -> IO CUInt
foreign export ccall
  cpex_event_string :: EventPtr -> Ptr CWString -> Ptr CUChar -> IO ()
foreign export ccall
  cpex_events_string :: SessionPtr -> Ptr EventPtr -> CUInt -> Ptr CWString ->
    IO CUInt
foreign export ccall
  cpex_event_equal :: EventPtr -> EventPtr -> IO Bool
foreign export ccall
  cpex_process_string :: SessionPtr -> ProcPtr -> Bool -> Ptr CWString -> IO CUInt
foreign export ccall
  cpex_process_equal :: ProcPtr -> ProcPtr -> IO Bool
foreign export ccall
  cpex_process_hash :: ProcPtr -> IO CUInt
foreign export ccall
  cpex_process_operator :: SessionPtr -> ProcPtr -> Ptr CUChar -> IO CUInt
foreign export ccall
  cpex_op_event :: SessionPtr -> ProcPtr -> Ptr EventPtr -> IO CUInt
foreign export ccall
  cpex_op_events :: SessionPtr -> ProcPtr -> Ptr (Ptr EventPtr) -> Ptr CUInt ->
    IO CUInt
foreign export ccall
  cpex_op_event_map :: SessionPtr -> ProcPtr -> Ptr (Ptr EventPtr) ->
    Ptr (Ptr EventPtr) -> Ptr CUInt -> IO CUInt
foreign export ccall
  cpex_op_alphabets :: SessionPtr -> ProcPtr -> Ptr (Ptr (Ptr EventPtr)) ->
    Ptr (Ptr CUInt) -> IO CUInt
foreign export ccall
  cpex_op_process :: SessionPtr -> ProcPtr -> Ptr ProcPtr -> IO CUInt
foreign export ccall
  cpex_op_process2 :: SessionPtr -> ProcPtr -> Ptr ProcPtr -> Ptr ProcPtr -> IO CUInt
foreign export ccall
  cpex_op_processes :: SessionPtr -> ProcPtr -> Ptr (Ptr ProcPtr) -> Ptr CUInt ->
    IO CUInt
foreign export ccall
  cpex_op_proccall :: SessionPtr -> ProcPtr -> Ptr ProcPtr -> Ptr CWString ->
    IO CUInt

foreign export ccall
  cpex_proccall_names :: SessionPtr -> Ptr (Ptr CWString) -> Ptr (Ptr CUInt) ->
    Ptr CUInt -> IO CUInt
foreign export ccall
  cpex_expression_value :: SessionPtr -> CWString -> Ptr ProcPtr -> IO CUInt
foreign export ccall
  cpex_string_to_event :: SessionPtr -> CWString -> Ptr EventPtr -> IO CUInt

-- Builtin process STOP.
csp_stop_id = procName (scopeId (builtInName "STOP") [] Nothing)
csp_stop = PProcCall csp_stop_id (POp PExternalChoice S.empty)

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
operatorNum (POp (PSynchronisingExternalChoice _) _) = 14
operatorNum (PBinaryOp (PSynchronisingInterrupt _) _ _) = 15
operatorNum (PProcCall _ _) = 16

-- Helper functions for reading from and writing to stable pointers.
class Transferrable a where
  input :: StablePtr a -> NativeSessionMonad a
  output :: a -> Ptr (StablePtr a) -> NativeSessionMonad ()
  outputArray :: [a] -> Ptr (Ptr (StablePtr a)) -> NativeSessionMonad ()

  input p = liftIO $ deRefStablePtr p

instance Transferrable UProc where
  output x o = liftIO $ newStablePtr x >>= poke o
  outputArray xs o = liftIO $ mapM newStablePtr xs >>= newArray >>= poke o

instance Transferrable Event where
  output x o = do
    t <- gets events
    liftIO $ pullevent t x >>= poke o
  outputArray xs o = do
    t <- gets events
    liftIO $ mapM (pullevent t) xs >>= newArray >>= poke o

pullevent :: (HashTable Event EventPtr) -> Event -> IO EventPtr
pullevent t e = do
  p <- H.lookup t e
  case p of
    Just ptr -> return ptr
    Nothing -> do
      ptr <- newStablePtr e
      H.insert t e ptr
      return ptr

-- Input: Session pointer.
--        Stable pointer to a process.
--        Boolean: False = synchronous (FDR 2 style) termination semantics,
--                 True = asynchronous (omega style).
-- Output: Array of events offered by the process (may include duplicates).
--         Array of processes resulting from applying the respective event.
--         Size of array.
cpex_transitions :: SessionPtr -> ProcPtr -> Bool -> Ptr (Ptr EventPtr) ->
  Ptr (Ptr ProcPtr) -> Ptr CUInt -> IO CUInt
cpex_transitions sessPtr inProc inOmega outEvents outProcs outCount =
  runSession sessPtr $ do
    p <- input inProc
    o <- gets omega
    let (es, pns) = unzip $ map (simplify (if inOmega then o else csp_stop)) $
          transitions (if inOmega then Just o else Nothing) p
    outputArray es outEvents
    outputArray pns outProcs
    liftIO $ poke outCount $ fromIntegral $ length es
    where simplify o (Tick, _) = (Tick, o)
          simplify _ x = x

-- Input: Event.
-- Output: A string representation of that event, suitable for output.
--         The type of the event (user defined, Tau or Tick).
cpex_event_string :: EventPtr -> Ptr CWString -> Ptr CUChar -> IO ()
cpex_event_string inEvent outName outType = do
  e <- deRefStablePtr inEvent
  name <- (newCWString . show . prettyPrint) e
  poke outType $ fromIntegral $ etype e
  poke outName name
  where etype Tau = 1
        etype Tick = 2
        etype _ = 0

-- Input: Session pointer.
--        List of events.
--        Size of that list.
-- Output: A string representation of that list, shortened wherever possible.
cpex_events_string :: SessionPtr -> Ptr EventPtr -> CUInt -> Ptr CWString -> IO CUInt
cpex_events_string sess inEvents inCount outName = runSession sess $ do
  events <- liftIO $ peekArray (fromIntegral inCount) inEvents >>= mapM deRefStablePtr
  doc <- M.prettyPrint events
  liftIO $ (newCWString . (renderStyle style{mode=LeftMode})) doc >>= poke outName

-- Input: Two events.
-- Returns: True if the input events are equal, false if not.
cpex_event_equal :: EventPtr -> EventPtr -> IO Bool
cpex_event_equal inEv1 inEv2 = do
  e1 <- deRefStablePtr inEv1
  e2 <- deRefStablePtr inEv2
  return (e1 == e2)

-- Input: Process.
-- Output: A string representation of that process, suitable for output.
cpex_process_string :: SessionPtr -> ProcPtr -> Bool -> Ptr CWString -> IO CUInt
cpex_process_string sessPtr inProc inBrief outString = runSession sessPtr $ do
  doc <- input inProc >>= (if inBrief then M.prettyPrintBrief else M.prettyPrint)
  liftIO $ (newCWString . (renderStyle style{mode=LeftMode})) doc >>= poke outString

-- Input: Two processes.
-- Returns: True if the input processes are equal, false if not.
cpex_process_equal :: ProcPtr -> ProcPtr -> IO Bool
cpex_process_equal inProc1 inProc2 = do
  p1 <- deRefStablePtr inProc1
  p2 <- deRefStablePtr inProc2
  return (p1 == p2)

-- Input: A process.
-- Returns: A hash of that process.
cpex_process_hash :: ProcPtr -> IO CUInt
cpex_process_hash inProc = do
  p <- deRefStablePtr inProc
  return $ fromIntegral $ hash p

-- Input: Process.
-- Output: A number representing the operator used.
cpex_process_operator :: SessionPtr -> ProcPtr -> Ptr CUChar -> IO CUInt
cpex_process_operator sessPtr inProc outOp = runSession sessPtr $ do
  p <- input inProc
  -- Force evaluation of p in case an error exists but has not evaluated yet.
  proc <- if p == p then return p else return p
  liftIO $ poke outOp $ fromIntegral $ operatorNum proc

-- Input: Process.
-- Output: The single event parameter for the process. Only valid for Prefix
--         processes (a -> P).
-- Error: If the process is of the wrong type.
cpex_op_event :: SessionPtr -> ProcPtr -> Ptr EventPtr -> IO CUInt
cpex_op_event sessPtr inProc outEvent = runSession sessPtr $ do
  p <- input inProc
  let e = get_event p
  case e of
    Just ev -> do
      output ev outEvent
    Nothing -> panic "The input process is not a prefix operator."
  where get_event (PUnaryOp (PPrefix ev) _) = Just ev
        get_event _ = Nothing

-- Input: Process.
-- Output: The set of events parameter for the process, and the number of events
--         in the set. Only valid for Exception (P [|A|> Q), Generalized
--         Parallel (P [|A|] Q), Hide (P \ A), Synchronising External Choice
--         (P [+ A +] Q) and Synchronising Interrupt (P /+ A +\ Q).
-- Error: If the process is of the wrong type.
cpex_op_events :: SessionPtr -> ProcPtr -> Ptr (Ptr EventPtr) -> Ptr CUInt ->
  IO CUInt
cpex_op_events sessPtr inProc outEvents outSize = runSession sessPtr $ do
  p <- input inProc
  let es = get_events p
  case es of
    Just es -> do
      outputArray (F.toList es) outEvents
      liftIO $ poke outSize $ fromIntegral $ S.length es
    Nothing -> panic "The input process does not take a set of events."
  where get_events (PBinaryOp (PException evs) _ _) = Just evs
        get_events (POp (PGenParallel evs) _) = Just evs
        get_events (PUnaryOp (PHide evs) _) = Just evs
        get_events (POp (PSynchronisingExternalChoice evs) _) = Just evs
        get_events (PBinaryOp (PSynchronisingInterrupt evs) _ _) = Just evs
        get_events _ = Nothing

-- Input: Process.
-- Output: Two lists of events, representing pairs (e1, e2) of events in the
--         event map, and the number of such pairs. Only valid for Linked
--         Parallel (P [A <- B] Q) and Rename (P [[A <- B]]).
-- Error: If the process is of the wrong type.
cpex_op_event_map :: SessionPtr -> ProcPtr -> Ptr (Ptr EventPtr) ->
  Ptr (Ptr EventPtr) -> Ptr CUInt -> IO CUInt
cpex_op_event_map sessPtr inProc outLeft outRight outSize = runSession sessPtr $ do
  p <- input inProc
  let em = get_event_map p
  case em of
    Just em -> do
      let eml = F.toList em
      outputArray (map fst eml) outLeft
      outputArray (map snd eml) outRight
      liftIO $ poke outSize $ fromIntegral $ S.length em
    Nothing -> panic "The input process does not take an event map."
  where get_event_map (PBinaryOp (PLinkParallel evm) _ _) = Just evm
        get_event_map (PUnaryOp (PRename evm) _) = Just evm
        get_event_map _ = Nothing

-- Input: Process.
-- Output: A list of lists of events, and a list of sizes for each such list.
--         Only valid for Alphabetized Parallel (P [A||B] Q). The size of each
--         list is equivalent to the number of events in the corresponding
--         process, and the number of lists equal to the number of processes.
-- Error: If the process is of the wrong type.
cpex_op_alphabets :: SessionPtr -> ProcPtr -> Ptr (Ptr (Ptr EventPtr)) ->
  Ptr (Ptr CUInt) -> IO CUInt
cpex_op_alphabets sessPtr inProc outAlphas outSizes = runSession sessPtr $ do
  p <- input inProc
  t <- gets events
  let as = get_alphas p
  case as of
    Just as -> liftIO $ do
      mapM ((mapM (pullevent t)) . F.toList) (F.toList as)
        >>= mapM newArray
        >>= newArray
        >>= poke outAlphas
      sizesPtr <- newArray $ (map (fromIntegral . S.length) . F.toList) as
      poke outSizes sizesPtr
    Nothing -> panic "The input process does not take alphabets."
  where get_alphas (POp (PAlphaParallel as) _) = Just as
        get_alphas _ = Nothing

-- Input: PUnaryOp process.
-- Output: The process contained within the operator.
-- Error: If the process is not PUnaryOp.
cpex_op_process :: SessionPtr -> ProcPtr -> Ptr ProcPtr -> IO CUInt
cpex_op_process sessPtr inProc outProc = runSession sessPtr $ do
  p <- input inProc
  let pn = next p
  case pn of
    Just pn -> do
      output pn outProc
    Nothing -> panic "The input process is not a unary operator."
  where next (PUnaryOp _ pn) = Just pn
        next _ = Nothing

-- Input: PBinaryOp process.
-- Output: The two processes contained within the operator.
-- Error: If the process is not PBinaryOp.
cpex_op_process2 :: SessionPtr -> ProcPtr -> Ptr ProcPtr -> Ptr ProcPtr -> IO CUInt
cpex_op_process2 sessPtr inProc outLeft outRight = runSession sessPtr $ do
  p <- input inProc
  let pn = next p
  case pn of
    Just (p1, p2) -> do
      output p1 outLeft
      output p2 outRight
    Nothing -> panic "The input process is not a binary operator."
  where next (PBinaryOp _ p1 p2) = Just (p1, p2)
        next _ = Nothing

-- Input: POp process.
-- Output: List of processes contained within the operator.
--         Size of the list.
-- Error: If the process is not POp.
cpex_op_processes :: SessionPtr -> ProcPtr -> Ptr (Ptr ProcPtr) -> Ptr CUInt ->
  IO CUInt
cpex_op_processes sessPtr inProc outProcs outSize = runSession sessPtr $ do
  p <- input inProc
  let pn = next p
  case pn of
    Just pn -> do
      outputArray (F.toList pn) outProcs
      liftIO $ poke outSize $ fromIntegral $ S.length pn
    Nothing -> panic "The input process is not an n-ary operator."
  where next (POp _ pn) = Just pn
        next _ = Nothing

-- Input: PProcCall process.
-- Output: The expansion of the process.
--         The string representation of the process.
-- Error: If the process is not PProcCall.
cpex_op_proccall :: SessionPtr -> ProcPtr -> Ptr ProcPtr -> Ptr CWString -> IO CUInt
cpex_op_proccall sessPtr inProc outProc outName = runSession sessPtr $ do
  p <- input inProc
  let pn = next p
  case pn of
    Just (n, pn) -> do
      output pn outProc
      name <- liftIO $ (newCWString . show . prettyPrint) n
      liftIO $ poke outName name
    Nothing -> panic "The input process is not a ProcCall."
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
  VProc proc <- stringToValue TProc name
  -- Force evaluation of proc in case an error exists but has not evaluated yet.
  evaluated <- if proc == proc then return proc else return proc
  output evaluated outProc

-- Input: CSPM session.
--        String representing an event.
-- Output: The event represented by that string.
-- Error: If the given string does not resolve to an event.
cpex_string_to_event :: SessionPtr -> CWString -> Ptr EventPtr -> IO CUInt
cpex_string_to_event sessPtr inName outEvent = runSession sessPtr $ do
  name <- liftIO $ peekCWString inName
  ev <- getEvent name
  -- Force evaluation of ev in case an error exists but has not evaluated yet.
  evaluated <- if ev == ev then return ev else return ev
  output evaluated outEvent
  where
    getEvent "_tick" = return Tick
    getEvent "_tau" = return Tau
    getEvent e = stringToValue TEvent e >>= return . UserEvent
