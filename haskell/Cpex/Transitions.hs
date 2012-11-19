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

-- Given a parallelized process, a list of sorted event alphabets, the result of
-- applying fmap transitions to the input process (passed manually for efficiency)
-- and a sorted list of events, returns a list of all (ev, p) pairs such that e can
-- be applied to the input process, assuming the only allowed events are those
-- listed in the alphabets, and p is the resulting process.
synchronize :: UProc -> S.Seq (S.Seq Event) -> S.Seq [(Event, UProc)] -> [Event] ->
  [(Event, UProc)]
synchronize (POp op ps) as tss [] = []
synchronize (POp op ps) as tss (ev:evs)
  | isNothing ps' = synchronize (POp op ps) as' tss' evs
  | otherwise = (ev, POp op (fromJust ps')):(synchronize (POp op ps) as' tss' evs)
  where (ps', as', tss') = reduce ev (S.viewl ps) (S.viewl as) (S.viewl tss)
        -- The reduce function constructs a candidate result process for a particular
        -- event, reducing the alphabets and transitions list of each process while
        -- doing so (like merging arbitrarily many sorted lists). If it determines
        -- that the event must be blocked, the sequence of processes to be
        -- synchronized together becomes Nothing.

        -- End of the list of processes.
        reduce ev S.EmptyL _ _ = (Just S.empty, S.empty, S.empty)
        -- Iterative step: got a process.
        reduce ev (p S.:< ps'') (a S.:< as'') (ts S.:< tss'')
          | isNothing p' = (Nothing, a' S.<| as'', ts' S.<| tss'')
          | isNothing ps''' = (Nothing, a' S.<| as''', ts' S.<| tss''')
          | otherwise = (Just ((fromJust p') S.<| (fromJust ps''')), a' S.<| as''', ts' S.<| tss''')
          where (p', a', ts') = reduce' ev (p, S.viewl a, ts)
                (ps''', as''', tss''') = reduce ev (S.viewl ps'') (S.viewl as'') (S.viewl tss'')
        -- Empty alphabet => No event in this branch.
        reduce' ev (p, S.EmptyL, ts) = (Just p, S.empty, ts)
        -- No transitions:
        reduce' ev (p, e S.:< a, [])
          -- Event too small => Go higher.
          | e < ev = reduce' ev (p, S.viewl a, [])
          -- Event too large => No event in this branch.
          | e > ev = (Just p, e S.<| a, [])
          -- Event matches => Block this event.
          | otherwise = (Nothing, e S.<| a, [])
        -- At least one event in alphabet, and at least one transition:
        reduce' ev (p, e S.:< a, (en, pn):ts)
          -- Event too small => Go higher.
          | e < ev = reduce' ev (p, S.viewl a, (en, pn):ts)
          -- Event too large => No event in this branch.
          | e > ev = (Just p, e S.<| a, (en, pn):ts)
          -- Transition too small => Go higher.
          | en < ev = reduce' ev (p, e S.:< a, ts)
          -- Transition too large => Block this event.
          | en > ev = (Nothing, a, (en, pn):ts)
          -- Transition and event both match => Perform event.
          | otherwise = (Just pn, e S.<| a, ts)

-- Given a function f taking a boolean and an (Event, UProc) pair, a sorted
-- sequence of events evs and a list of transitions (sorted by Event), runs f b
-- on each transition (ev, p) where b is the indicator of whether ev appears in
-- evs.
mapHas :: (Bool -> (Event, UProc) -> a) -> S.Seq Event -> [(Event, UProc)] -> [a]
mapHas f evs ts = mapHas' f (S.viewl evs) ts
  where mapHas' f _ [] = []
        mapHas' f S.EmptyL (t:ts) = (f False t):(mapHas f S.empty ts)
        mapHas' f (e S.:< evs) ((ev,p):ts)
          | e < ev = mapHas f evs ((ev,p):ts)
          | e > ev = (f False (ev,p)):(mapHas f (e S.<| evs) ts)
          | otherwise = (f True (ev,p)):(mapHas f (e S.<| evs) ts)

-- The transitions for an alphabetized parallel process are the transitions
-- resulting from the events offered by each process in parallel, minus those
-- resulting from events on which we are synchronizing that at least one process
-- syncing on that event does not offer.
transitions (POp (PAlphaParallel as) ps) = frees ++ syncs
  -- All events involved in synchronization. Don't want duplication - use a set.
  where sevs = F.foldr (\a evs -> F.foldr Set.insert evs a) Set.empty as'
  -- Only Tau events are free, those not appearing in the alphabet are blocked.
  -- Since all events in frees are Tau, which is the "least" event, no need to
  -- sort this or the result.
        frees = sort . filter ((== Tau) . fst) $ transitionsMap alphaPar ps
        alphaPar n (ev, pn) = (ev, POp (PAlphaParallel as) (S.update n pn ps))
  -- Other events: allow only if all processes that synchronize on the event
  -- offer it.
        syncs = synchronize (POp (PAlphaParallel as) ps) as' tss (Set.toAscList sevs)
        tss = fmap transitions ps
        as' = fmap (S.unstableSort . (Tick S.<|)) as

-- The transitions for a process with an exception handler are the events that
-- that process may perform, with any event to be caught handing control to the
-- handler process.
transitions (PBinaryOp (PException evs) p1 p2) = map handle $ transitions p1
  where handle (ev, pn)
          | F.elem ev evs = (ev, p2)
          | otherwise     = (ev, PBinaryOp (PException evs) pn p2)

-- The transitions for external choice are to perform some event and move to the
-- resulting process, and the processes resulting from each tau event.
transitions (POp PExternalChoice ps) = sort $ transitionsMap fixTau ps
  where fixTau n (Tau, pn) = (Tau, POp PExternalChoice (S.update n pn ps))
        fixTau _ (ev, pn) = (ev, pn)

-- The transitions for a generalized parallel process are the free events
-- performed by each process independently, plus the synchronized events that
-- are offered by all processes.
transitions (POp (PGenParallel evs) ps) = sort $ frees ++ syncs
  -- Free events: anything not in evs.
  where frees = filterFree (S.viewl evs') $ sort $ transitionsMap genPar ps
        filterFree _ [] = []
        filterFree S.EmptyL ts = ts
        filterFree (e S.:< es) ((en, pn):ts)
          | e < en = filterFree (S.viewl es) ((en, pn):ts)
          | e > en = (en, pn):(filterFree (e S.:< es) ts)
          | otherwise = filterFree (e S.:< es) ts
        genPar n (ev, pn) = (ev, POp (PGenParallel evs) (S.update n pn ps))
  -- The definition of syncs here is almost identical to that in alphabetized
  -- parallel, except this time we only have one set of events to worry about.
        syncs = synchronize (POp (PGenParallel evs) ps) as tss (F.toList evs')
        evs' = S.unstableSort (Tick S.<| evs)
        tss = fmap transitions ps
        as = S.replicate (S.length ps) evs'

-- The transitions for a process with hidden events are the transitions of that
-- process, with the hidden events replaced by tau.
transitions (PUnaryOp (PHide evs) p) =
  sort $ mapHas hideTau (S.unstableSort evs) $ transitions p
  where hideTau x (ev, p) = (if x then Tau else ev, PUnaryOp (PHide evs) p)

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
          | tickProcs == [] = ts
          | otherwise       = ts1 ++ tickProcs ++ ts2
          where (ts1, ts2) = partition ((== Tau) . fst) ts
        tickProcs = synchronize (POp PInterleave ps)
          (S.replicate (S.length ps) (S.singleton Tick)) (fmap transitions ps) [Tick]

-- The transitions for an interrupt are the events offered by p1 plus those
-- offered by p2, with the processes resulting from an event in p1 replaced by
-- an equivalent process interruptable by p2.
transitions (PBinaryOp PInterrupt p1 p2) = sort $
  (map (\(ev, pn) -> (ev, PBinaryOp PInterrupt pn p2)) (transitions p1)) ++
  (map tauOrInterrupt (transitions p2))
  where tauOrInterrupt (Tau, pn) = (Tau, PBinaryOp PInterrupt p1 pn)
        tauOrInterrupt x = x

-- The transitions for a link parallel process are all free events plus those
-- event pairs in the mapping where each side is offered by the respective
-- process.
transitions (PBinaryOp (PLinkParallel evm) p1 p2) = sort $ frees ++ syncs
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
transitions (PUnaryOp (PRename evm) p) = sort $
  map (\(ev, pn) -> (fromMaybe ev (lookup ev (F.toList evm)),
    PUnaryOp (PRename evm) pn)) $ transitions p

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
