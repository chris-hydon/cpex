{-# LANGUAGE ForeignFunctionInterface #-}
module Cpex.Transitions (
    transitions
) where

import Control.Monad.Trans
import CSPM
import CSPM.Foreign
import CSPM.Prelude
import Data.IORef
import Data.List
import Data.Maybe
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Util.PrettyPrint

import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Map as M

type ProcPtr = StablePtr (IORef UProc)
type EventPtr = StablePtr (IORef Event)

-- Foreign exports: Each Ptr argument (not StablePtr) is intended for output.
foreign export ccall
  cpex_transitions :: ProcPtr -> Ptr (Ptr EventPtr) -> Ptr (Ptr ProcPtr) ->
    Ptr CUInt -> IO ()
foreign export ccall
  cpex_event_string :: EventPtr -> Ptr CWString -> Ptr CUChar -> IO ()
foreign export ccall
  cpex_process_string :: ProcPtr -> Ptr CWString -> IO ()
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

foreign export ccall
  cpex_event_free :: EventPtr -> IO ()
foreign export ccall
  cpex_process_free :: ProcPtr -> IO ()

-- Input: Stable pointer to a process.
-- Output: Array of events offered by the process (may include duplicates).
--         Array of processes resulting from applying the respective event.
--         Size of array.
cpex_transitions :: ProcPtr -> Ptr (Ptr EventPtr) -> Ptr (Ptr ProcPtr) ->
  Ptr CUInt -> IO ()
cpex_transitions inProc outEvents outProcs outCount = do
  pRef <- deRefStablePtr inProc
  p <- readIORef pRef
  let (es, pns) = unzip $ map simplify $ transitions p
  eRefs <- mapM newIORef es
  pnRefs <- mapM newIORef pns
  ePtrs <- mapM newStablePtr eRefs
  pnPtrs <- mapM newStablePtr pnRefs
  eArray <- newArray ePtrs
  pnArray <- newArray pnPtrs
  poke outEvents eArray
  poke outProcs pnArray
  liftIO $ poke outCount $ fromIntegral $ length es

-- Input: Event.
-- Output: A string representation of that event, suitable for output.
--         The type of the event (user defined, Tau or Tick).
cpex_event_string :: EventPtr -> Ptr CWString -> Ptr CUChar -> IO ()
cpex_event_string inEvent outName outType = do
  eRef <- deRefStablePtr inEvent
  e <- readIORef eRef
  name <- (newCWString . show . prettyPrint) e
  poke outType $ fromIntegral $ etype e
  poke outName name
  where etype Tau = 1
        etype Tick = 2
        etype _ = 0

-- Input: Process.
-- Output: A string representation of that process, suitable for output.
cpex_process_string :: ProcPtr -> Ptr CWString -> IO ()
cpex_process_string inProc outString = do
  pRef <- deRefStablePtr inProc
  p <- readIORef pRef
  name <- (newCWString . show . prettyPrint) p
  poke outString name

-- Input: Process.
-- Output: A number representing the operator used.
cpex_process_operator :: ProcPtr -> Ptr CUChar -> IO ()
cpex_process_operator inProc outOp = do
  pRef <- deRefStablePtr inProc
  p <- readIORef pRef
  poke outOp $ fromIntegral $ operatorNum p

-- Input: Process.
-- Output: The single event parameter for the process. Only valid for Prefix
--         processes (a -> P).
-- Error: If the process is of the wrong type.
cpex_op_event :: ProcPtr -> Ptr EventPtr -> IO CUInt
cpex_op_event inProc outEvent = do
  pRef <- deRefStablePtr inProc
  p <- readIORef pRef
  let e = get_event p
  case e of
    Just ev -> liftIO $ do
      eRef <- newIORef ev
      ePtr <- newStablePtr eRef
      poke outEvent ePtr
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
  pRef <- deRefStablePtr inProc
  p <- readIORef pRef
  let es = get_events p
  case es of
    Just es -> liftIO $ do
      eRefs <- mapM newIORef $ F.toList es
      ePtrs <- mapM newStablePtr eRefs
      arrayPtr <- newArray ePtrs
      poke outEvents arrayPtr
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
  pRef <- deRefStablePtr inProc
  p <- readIORef pRef
  let em = get_event_map p
  case em of
    Just em -> liftIO $ do
      let eml = F.toList em
      eLeftRefs <- mapM (newIORef . fst) $ eml
      eRightRefs <- mapM (newIORef . snd) $ eml
      eLeftPtrs <- mapM newStablePtr eLeftRefs
      eRightPtrs <- mapM newStablePtr eRightRefs
      leftPtr <- newArray eLeftPtrs
      rightPtr <- newArray eRightPtrs
      poke outLeft leftPtr
      poke outRight rightPtr
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
  pRef <- deRefStablePtr inProc
  p <- readIORef pRef
  let as = get_alphas p
  case as of
    Just as -> liftIO $ do
      aRefs <- mapM ((mapM newIORef) . F.toList) $ F.toList as
      aPtrs <- mapM (mapM newStablePtr) aRefs
      arrayPtrs <- mapM newArray aPtrs
      arrayPtr <- newArray arrayPtrs
      poke outAlphas arrayPtr
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
  pRef <- deRefStablePtr inProc
  p <- readIORef pRef
  let pn = next p
  case pn of
    Just pn -> liftIO $ do
      pRef <- newIORef pn
      pPtr <- newStablePtr pRef
      poke outProc pPtr
      return 1
    Nothing -> return 0
  where next (PUnaryOp _ pn) = Just pn
        next _ = Nothing

-- Input: PBinaryOp process.
-- Output: The two processes contained within the operator.
-- Error: If the process is not PBinaryOp.
cpex_op_process2 :: ProcPtr -> Ptr ProcPtr -> Ptr ProcPtr -> IO CUInt
cpex_op_process2 inProc outLeft outRight = do
  pRef <- deRefStablePtr inProc
  p <- readIORef pRef
  let pn = next p
  case pn of
    Just (p1, p2) -> liftIO $ do
      p1Ref <- newIORef p1
      p1Ptr <- newStablePtr p1Ref
      p2Ref <- newIORef p2
      p2Ptr <- newStablePtr p2Ref
      poke outLeft p1Ptr
      poke outRight p2Ptr
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
  pRef <- deRefStablePtr inProc
  p <- readIORef pRef
  let pn = next p
  case pn of
    Just pn -> liftIO $ do
      pRef <- mapM newIORef (F.toList pn)
      pPtr <- mapM newStablePtr pRef
      arrayPtr <- newArray pPtr
      poke outProcs arrayPtr
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
  pRef <- deRefStablePtr inProc
  p <- readIORef pRef
  let pn = next p
  case pn of
    Just (n, pn) -> liftIO $ do
      pRef <- newIORef pn
      pPtr <- newStablePtr pRef
      poke outProc pPtr
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
  procRef <- liftIO $ newIORef expressionValue
  procPtr <- liftIO $ newStablePtr procRef
  liftIO $ poke outProc procPtr

-- Memory freeing functions.
cpex_event_free :: EventPtr -> IO ()
cpex_event_free = freeStablePtr
cpex_process_free :: ProcPtr -> IO ()
cpex_process_free = freeStablePtr

-- Builtin process STOP.
builtInName s = name . head . filter ((== s) . stringName) $ builtins False
csp_stop_id = procName (scopeId (builtInName "STOP") [] Nothing)
csp_stop = PProcCall csp_stop_id (POp PExternalChoice S.empty)

-- Given a transition, replaces the process with STOP if the event is Tick.
simplify :: (Event, UProc) -> (Event, UProc)
simplify (Tick, _) = (Tick, csp_stop)
simplify x = x

-- | Given a process, returns a list of transitions that the process has,
-- represented as a pair of (event, resulting process)
transitions :: UProc -> [(Event, UProc)]

-- Given a function (Int -> (Event, UProc) -> (Event, UProc) and a sequence of
-- UProcs, finds all transitions available to each UProc in the sequence and
-- applies the function to each, returning a list of resulting (Event, UProc)
-- pairs.
transitionsMap :: (Int -> (Event, UProc) -> (Event, UProc)) -> S.Seq UProc ->
                  [(Event, UProc)]
transitionsMap f = F.concat . (S.mapWithIndex (\n p -> map (f n) (transitions p)))

-- Given a parallelized process, a list of alphabets and an event, returns
-- Maybe the (Event, p) pair where p is the result of applying the event to the
-- input process, if that event is part of the synchronization and can be
-- applied (returns Nothing if the event is free or if the event is not offered
-- by all interested processes).
synchronize :: UProc -> S.Seq (S.Seq Event) -> Event -> Maybe (Event, UProc)
synchronize (POp op ps) as ev = F.foldr (reduce ev) (Just (ev, POp op ps))
              (S.zip3 (S.fromList [0..(S.length as)]) as ps)
  -- The reduce function checks that the given process/alphabet either ignores
  -- or provides the event. In the former case, the event does not modify the
  -- state of that process. In the latter, the event occurs within that process.
  -- If neither is true, the event is blocked so return Nothing.
  where reduce _ _ Nothing = Nothing
        reduce ev (n, evs, p) (Just (e, POp op qs)) -- ev == e
          | F.notElem ev evs = Just (e, POp op qs)
          | isNothing lookup = Nothing
          | otherwise = Just (e, POp op (S.update n (fromJust lookup) qs))
          where lookup = foldr (\(e, pn) x -> if e == ev then Just pn else x)
                  Nothing (transitions p)

-- The transitions for an alphabetized parallel process are the transitions
-- resulting from the events offered by each process in parallel, minus those
-- resulting from events on which we are synchronizing that at least one process
-- syncing on that event does not offer.
transitions (POp (PAlphaParallel as) ps) = frees ++ syncs
  -- All events involved in synchronization. Don't want duplication - use a set.
  where sevs = F.foldr (\a evs -> F.foldr Set.insert evs a) Set.empty as'
  -- Only Tau events are free, those not appearing in the alphabet are blocked.
        frees = filter ((== Tau) . fst) $ transitionsMap alphaPar ps
        alphaPar n (ev, pn) = (ev, POp (PAlphaParallel as) (S.update n pn ps))
  -- Other events: allow only if all processes that synchronize on the event
  -- offer it. Map each event to Just the resulting transition, or Nothing if
  -- the event is blocked.
        syncs = mapMaybe (synchronize (POp (PAlphaParallel as) ps) as')
          (Set.toList sevs)
        as' = fmap (Tick S.<|) as

-- The transitions for a process with an exception handler are the events that
-- that process may perform, with any event to be caught handing control to the
-- handler process.
transitions (PBinaryOp (PException evs) p1 p2) = map handle $ transitions p1
  where handle (ev, pn)
          | F.elem ev evs = (ev, p2)
          | otherwise     = (ev, PBinaryOp (PException evs) pn p2)

-- The transitions for external choice are to perform some event and move to the
-- resulting process, and the processes resulting from each tau event.
transitions (POp PExternalChoice ps) = transitionsMap fixTau ps
  where fixTau n (Tau, pn) = (Tau, POp PExternalChoice (S.update n pn ps))
        fixTau _ (ev, pn) = (ev, pn)

-- The transitions for a generalized parallel process are the free events
-- performed by each process independently, plus the synchronized events that
-- are offered by all processes.
transitions (POp (PGenParallel evs) ps) = frees ++ syncs
  -- Free events: anything not in evs.
  where frees = filter (((flip F.notElem) evs') . fst) $ transitionsMap genPar ps
        genPar n (ev, pn) = (ev, POp (PGenParallel evs) (S.update n pn ps))
  -- The definition of syncs here is almost identical to that in alphabetized
  -- parallel, except this time it's slightly simpler because we only have one
  -- set of events to worry about.
        syncs = mapMaybe (synchronize (POp (PGenParallel evs) ps) as) $ F.toList evs'
        evs' = Tick S.<| evs
        as = S.replicate (S.length ps) evs

-- The transitions for a process with hidden events are the transitions of that
-- process, with the hidden events replaced by tau.
transitions (PUnaryOp (PHide evs) p) =
  map (\(ev, p) ->
    (if F.elem ev evs then Tau else ev,
    PUnaryOp (PHide evs) p)
  ) $ transitions p

-- The transitions for internal choice are to perform a tau event for each
-- branch.
transitions (POp PInternalChoice ps) = [(Tau, p) | p <- (F.toList ps)]

-- The transitions for an interleaved process are the transitions resulting from
-- the events offered by each process in parallel. Special handling needed for
-- Tick.
transitions (POp PInterleave ps) = maybeTick $
  filter ((/= Tick) . fst) $ transitionsMap interleave ps
  where interleave n (ev, pn) = (ev, POp PInterleave (S.update n pn ps))
        maybeTick ts
          | tickProcs == Nothing = ts
          | otherwise            = (fromJust tickProcs):ts
        tickProcs = synchronize (POp PInterleave ps)
          (S.replicate (S.length ps) (S.singleton Tick)) Tick

-- The transitions for an interrupt are the events offered by p1 plus those
-- offered by p2, with the processes resulting from an event in p1 replaced by
-- an equivalent process interruptable by p2.
transitions (PBinaryOp PInterrupt p1 p2) =
  (map (\(ev, pn) -> (ev, PBinaryOp PInterrupt pn p2)) (transitions p1)) ++
  (map tauOrInterrupt (transitions p2))
  where tauOrInterrupt (Tau, pn) = (Tau, PBinaryOp PInterrupt p1 pn)
        tauOrInterrupt x = x

-- The transitions for a link parallel process are all free events plus those
-- event pairs in the mapping where each side is offered by the respective
-- process.
transitions (PBinaryOp (PLinkParallel evm) p1 p2) = frees ++ syncs
  -- Free events: For p_i, those not equal to e_i for some (e1, e2) in evm.
  where (free1, sync1) = partition (isFree fst) $ transitions p1
        (free2, sync2) = partition (isFree snd) $ transitions p2
        isFree f (ev, _) = F.all ((/= ev) . f) evm'
        frees =
          (map (\(ev, pn) -> (ev, PBinaryOp (PLinkParallel evm) pn p2)) free1) ++
          (map (\(ev, pn) -> (ev, PBinaryOp (PLinkParallel evm) p1 pn)) free2)
  -- Synced events: Any (e1, e2) in evm such that e1 is offered by p1 and e2 is
  -- offered by p2. The event is then hidden. For Tick, the same rules apply as
  -- with other parallel operators: sync on Tick but do not hide it.
        syncs = mapMaybe (
            \((e1, p1), (e2, p2)) ->
              if e1 == Tick && e2 == Tick
                then Just (Tick, PBinaryOp (PLinkParallel evm) p1 p2)
              else if F.elem (e1, e2) evm
                then Just (Tau, PBinaryOp (PLinkParallel evm) p1 p2)
                else Nothing
          ) [(t1, t2) | t1 <- transitions p1, t2 <- transitions p2]
        evm' = (Tick,Tick) S.<| evm

-- Transitions of a chase-compressed process are those of the process after
-- silently and recursively following some tau if any exist.
transitions (PUnaryOp (POperator (Chase b)) p) = chase (transitions p)
  where chase ts
          | taus == [] = ts
          | otherwise  = chase $ transitions $ (snd . head) taus
          where taus = filter ((== Tau) . fst) ts

-- Other compressions are semantics-preserving, so the transitions are simply
-- those of the process itself.
transitions (PUnaryOp (POperator _) p) = transitions p

-- The only transition here is to perform the event and move to p
transitions (PUnaryOp (PPrefix ev) p) = [(ev, p)]

-- The transitions of a renamed process are the transitions of the original
-- process, with events mapped to the new process.
transitions (PUnaryOp (PRename evm) p) =
  map (\(ev, pn) -> (F.foldr (lookup ev) ev evm, PUnaryOp (PRename evm) pn)) $
    transitions p
  where lookup x (k, v) e
          | k == x    = v
          | otherwise = e

-- The transitions of the sequential composition p1; p2 are the transitions of
-- p1 with the resulting process sequentially composed with p2, for all events
-- except Tick, which results in a Tau to the process p2.
transitions (PBinaryOp PSequentialComp p1 p2) = map nextProcess $ transitions p1
  where nextProcess (ev, pn)
          | ev == Tick = (Tau, p2)
          | otherwise  = (ev, PBinaryOp PSequentialComp pn p2)

-- The transitions of a sliding choice are the transitions of p1 plus a tau
-- event which results in p2.
transitions (PBinaryOp PSlidingChoice p1 p2) = (Tau, p2) :
  (map slideVisible $ transitions p1)
  where slideVisible (Tau, pn) = (Tau, PBinaryOp PSlidingChoice pn p2)
        slideVisible x = x

-- The transitions for a process call are simply those of the inner process
transitions (PProcCall pn p) = transitions p


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
