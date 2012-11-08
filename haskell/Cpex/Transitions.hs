module Cpex.Transitions (
    transitions
) where

import CSPM
import Data.List
import Data.Maybe

import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Set as Set


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
              (S.zip4 (S.fromList [0..(S.length as)]) as ps (fmap transitions ps))
  -- The reduce function checks that the given process/alphabet either ignores
  -- or provides the event. In the former case, the event does not modify the
  -- state of that process. In the latter, the event occurs within that process.
  -- If neither is true, the event is blocked so return Nothing.
  where reduce _ _ Nothing = Nothing
        reduce ev (n, evs, p, ts) (Just (e, POp op qs)) -- ev == e
          | F.notElem ev evs = Just (e, POp op qs)
          | isNothing lookup = Nothing
          | otherwise = Just (e, POp op (S.update n (fromJust lookup) qs))
          where lookup = foldr (\(e, pn) x -> if e == ev then Just pn else x)
                  Nothing ts

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
        as = S.replicate (S.length ps) evs'

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
  where (free1, sync1) = partition (isFree fst) $ ts1
        (free2, sync2) = partition (isFree snd) $ ts2
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
          ) [(t1, t2) | t1 <- ts1, t2 <- ts2]
        ts1 = transitions p1
        ts2 = transitions p2
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
