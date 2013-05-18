module Cpex.Transitions (
    transitions
) where

import CSPM
import Data.List
import Data.Maybe
import Data.Ord

import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Set as Set

-- Given maybe Omega and given a process, returns a list of transitions that the
-- process has, represented as a pair of (event, resulting process). This supports
-- both synchronous (omega) termination semantics (STS) and asynchronous
-- (FDR 2 style) termination semantics (ATS). If Omega is given, use STS, otherwise
-- use ATS.
transitions :: Maybe UProc -> UProc -> [(Event, UProc)]

-- Given a function (Int -> (Event, UProc) -> (Event, UProc) and a sequence of
-- UProcs, finds all transitions available to each UProc in the sequence and
-- applies the function to each, returning a list of resulting (Event, UProc)
-- pairs.
transitionsMap :: (Int -> (Event, UProc) -> (Event, UProc)) -> Maybe UProc -> S.Seq UProc ->
                  [(Event, UProc)]
transitionsMap f o = (F.foldr tMerge []) .
  (S.mapWithIndex (\n p -> sortBy (comparing fst) $ map (f n) $ transitions o p))

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

-- Termination for ATS. Given maybe Omega, a sequence of processes in parallel and
-- all the available transitions except possibly for termination, returns the
-- transitions performed. This returns the input transitions unless we have an Omega
-- and all the processes are Omega.
term :: Maybe UProc -> S.Seq UProc -> [(Event, UProc)] -> [(Event, UProc)]
term Nothing _ ts = ts
term (Just o) ps ts
  | ps == S.empty = ts -- Special case - parallel on nothing should not offer tick.
  | allOmega ps = [(Tick, o)]
  | otherwise = ts
  where allOmega = F.foldr (\p b -> b && p == o) True

-- The transitions for an alphabetized parallel process are the transitions
-- resulting from the events offered by each process in parallel, minus those
-- resulting from events on which we are synchronizing that at least one process
-- syncing on that event does not offer.
transitions o (POp (PAlphaParallel as) ps) = term o ps $ frees ++ syncs
  -- All events involved in synchronization. Don't want duplication - use a set.
  where sevs = F.foldr (\a evs -> F.foldr Set.insert evs a) Set.empty as'
  -- Only Tau events are free, those not appearing in the alphabet are blocked.
  -- Since all events in frees are Tau, which is the "least" event, no need to
  -- sort this or the result. In ATS, Tick is hidden and free.
        frees = filter (isEventFree . fst) $ transitionsMap alphaPar o ps
        alphaPar n (ev, pn)
          | o == Nothing || ev /= Tick =
            (ev, POp (PAlphaParallel as) (S.update n pn ps))
          | otherwise = (Tau, POp (PAlphaParallel as) (S.update n (fromJust o) ps))
        isEventFree Tau = True
        isEventFree Tick = o /= Nothing
        isEventFree _ = False
  -- Other events: allow only if all processes that synchronize on the event
  -- offer it.
        syncs = synchronize (POp (PAlphaParallel as) ps) as' tss (Set.toAscList sevs)
        tss = fmap (transitions o) ps
  -- Sort the alphabet. We also add tick, which is implicit in STS. For ATS, due to
  -- the term function, we handle Ticks before we do anything else so this does not
  -- change anything.
        as' = fmap (S.unstableSort . (Tick S.<|)) as

-- The transitions for a process with an exception handler are the events that
-- that process may perform, with any event to be caught handing control to the
-- handler process.
transitions o (PBinaryOp (PException evs) p1 p2) = map handle $ transitions o p1
  where handle (ev, pn)
          | F.elem ev evs = (ev, p2)
          | otherwise     = (ev, PBinaryOp (PException evs) pn p2)

-- The transitions for external choice are to perform some event and move to the
-- resulting process, and the processes resulting from each tau event.
transitions o (POp PExternalChoice ps) = transitionsMap fixTau o ps
  where fixTau n (Tau, pn) = (Tau, POp PExternalChoice (S.update n pn ps))
        fixTau _ (ev, pn) = (ev, pn)

-- The transitions for a generalized parallel process are the free events
-- performed by each process independently, plus the synchronized events that
-- are offered by all processes.
transitions o (POp (PGenParallel evs) ps) = term o ps $
  foldr (insertBy (comparing fst)) frees syncs
  -- Free events: anything not in evs.
  where frees = filterFree (S.viewl evs') $ transitionsMap genPar o ps
        filterFree _ [] = []
        filterFree S.EmptyL ts = ts
        filterFree (e S.:< es) ((en, pn):ts)
          | e < en = filterFree (S.viewl es) ((en, pn):ts)
          | e > en = (en, pn):(filterFree (e S.:< es) ts)
          | otherwise = filterFree (e S.:< es) ts
        genPar n (ev, pn)
          | o == Nothing || ev /= Tick =
            (ev, POp (PGenParallel evs) (S.update n pn ps))
          | otherwise = (Tau, POp (PGenParallel evs) (S.update n (fromJust o) ps))
  -- The definition of syncs here is almost identical to that in alphabetized
  -- parallel, except this time we only have one set of events to worry about.
        syncs = synchronize (POp (PGenParallel evs) ps) as tss (F.toList evs')
        evs' = S.unstableSort (if o == Nothing then Tick S.<| evs else evs)
        tss = fmap (transitions o) ps
        as = S.replicate (S.length ps) evs'

-- The transitions for a process with hidden events are the transitions of that
-- process, with the hidden events replaced by tau.
transitions o (PUnaryOp (PHide evs) p) =
  sortBy (comparing fst) $ mapHas hideTau (S.unstableSort evs) $ transitions o p
  where hideTau x (ev, p) = (if x then Tau else ev, PUnaryOp (PHide evs) p)

-- The transitions for internal choice are to perform a tau event for each
-- branch.
transitions _ (POp PInternalChoice ps) = [(Tau, p) | p <- (F.toList ps)]

-- The transitions for an interleaved process are the transitions resulting from
-- the events offered by each process in parallel. Special handling needed for
-- Tick. This one is more straightforward to write in two cases depending on the
-- semantics used.
transitions Nothing (POp PInterleave ps) = maybeTick $
  filter ((/= Tick) . fst) $ transitionsMap interleave Nothing ps
  where interleave n (ev, pn) = (ev, POp PInterleave (S.update n pn ps))
        maybeTick ts
          | tickProcs == [] = ts
          | otherwise       = ts1 ++ tickProcs ++ ts2
          where (ts1, ts2) = partition ((== Tau) . fst) ts
        tickProcs = synchronize (POp PInterleave ps)
          (S.replicate (S.length ps) (S.singleton Tick))
          (fmap (transitions Nothing) ps) [Tick]

transitions (Just o) (POp PInterleave ps) =
  term (Just o) ps $ transitionsMap interleave (Just o) ps
  where interleave n (Tick, pn) = (Tau, POp PInterleave (S.update n o ps))
        interleave n (ev, pn) = (ev, POp PInterleave (S.update n pn ps))

-- The transitions for an interrupt are the events offered by p1 plus those
-- offered by p2, with the processes resulting from an event in p1 replaced by
-- an equivalent process interruptable by p2.
transitions o (PBinaryOp PInterrupt p1 p2) = tMerge
  (map (\(ev, pn) -> (ev, PBinaryOp PInterrupt pn p2)) (transitions o p1))
  (map tauOrInterrupt (transitions o p2))
  where tauOrInterrupt (Tau, pn) = (Tau, PBinaryOp PInterrupt p1 pn)
        tauOrInterrupt x = x

-- The transitions for a link parallel process are all free events plus those
-- event pairs in the mapping where each side is offered by the respective
-- process.
transitions o (PBinaryOp (PLinkParallel evm) p1 p2) =
  term o (p1 S.<| (S.singleton p2)) $ tMerge frees syncs
  -- Free events: For p_i, those not equal to e_i for some (e1, e2) in evm.
  where (free1, sync1) = partition (isFree fst) $ ts1
        (free2, sync2) = partition (isFree snd) $ ts2
        isFree f (ev, _) = F.all ((/= ev) . f) evm'
        frees = tMerge (map linkPar1 free1) (map linkPar2 free2)
        linkPar1 (ev, pn)
          | o == Nothing || ev /= Tick = (ev, PBinaryOp (PLinkParallel evm) pn p2)
          | otherwise = (Tau, PBinaryOp (PLinkParallel evm) (fromJust o) p2)
        linkPar2 (ev, pn)
          | o == Nothing || ev /= Tick = (ev, PBinaryOp (PLinkParallel evm) p1 pn)
          | otherwise = (Tau, PBinaryOp (PLinkParallel evm) p1 (fromJust o))
  -- Synced events: Any (e1, e2) in evm such that e1 is offered by p1 and e2 is
  -- offered by p2. The event is then hidden. For Tick (which we only encounter by
  -- this point in ATS), the same rules apply as with other parallel operators: sync
  -- on Tick but do not hide it.
        syncs = sortBy (comparing fst) $ mapMaybe (
            \((e1, p1), (e2, p2)) ->
              if e1 == Tick && e2 == Tick
                then Just (Tick, PBinaryOp (PLinkParallel evm) p1 p2)
              else if F.elem (e1, e2) evm
                then Just (Tau, PBinaryOp (PLinkParallel evm) p1 p2)
                else Nothing
          ) [(t1, t2) | t1 <- ts1, t2 <- ts2]
        ts1 = transitions o p1
        ts2 = transitions o p2
        evm' = if o == Nothing then (Tick,Tick) S.<| evm else evm

-- Transitions of a chase-compressed process are those of the process after
-- silently and recursively following some tau if any exist.
transitions o (PUnaryOp (POperator (Chase b)) p) = wrap $ chase 0 $ transitions o p
  where chase 1000 ts = []
        chase n ts
          | taus == [] = ts
          | otherwise  = chase (n+1) $ transitions o $ (snd . head) taus
          where taus = filter ((== Tau) . fst) ts
        wrap = map (\(ev, p) -> (ev, PUnaryOp (POperator (Chase b)) p))

-- Transitions of a prioritise-compressed process are those of the process whose
-- events occur in the first set containing events offered. Tau and tick are
-- implicitly in the first event set.
transitions o (PUnaryOp (POperator (Prioritise b evss)) p) =
  wrap $ prioritise (S.viewl evss) $ transitions o p
  where prioritise S.EmptyL = prioritise' (S.singleton (Tau S.<| Tick S.<| S.empty))
        prioritise (evs S.:< evss) = prioritise' ((Tau S.<| Tick S.<| evs) S.<| evss)
  -- Iterate from left to right: if some transition is found with its event in the
  -- current set of events, we keep only transitions with events in that set, plus
  -- those not in the union of evss.
        prioritise' evss' ts = tMerge (F.foldl (filterEvents ts) [] evss')
          (filter ((flip notElem $ affected) . fst) ts)
        affected = Tau:Tick:(F.concatMap F.toList evss)
        filterEvents ts [] evs = filter ((flip F.elem $ evs) . fst) ts
        filterEvents ts ts' evs = ts'
        wrap = map (\(ev, p) -> (ev, PUnaryOp (POperator (Prioritise b evss)) p))

-- Other compressions are semantics-preserving, so the transitions are simply
-- those of the process itself.
transitions o (PUnaryOp (POperator _) p) = transitions o p

-- The only transition here is to perform the event and move to p
transitions _ (PUnaryOp (PPrefix ev) p) = [(ev, p)]

-- The transitions of a renamed process are the transitions of the original
-- process, with events mapped to the new process. An event may be renamed to
-- multiple things at once.
transitions o (PUnaryOp (PRename evm) p) = sortBy (comparing fst) $
  concatMap (\(ev, pn) -> [(ev', PUnaryOp (PRename evm) pn) |
    ev' <- lookupMany ev (F.toList evm)]) $ transitions o p
  where lookupMany k m = let r = map snd (filter ((== k) . fst) m)
          in if r == [] then [k] else r

-- The transitions of the sequential composition p1; p2 are the transitions of
-- p1 with the resulting process sequentially composed with p2, for all events
-- except Tick, which results in a Tau to the process p2.
transitions o (PBinaryOp PSequentialComp p1 p2) = map nextProcess $ transitions o p1
  where nextProcess (ev, pn)
          | ev == Tick = (Tau, p2)
          | otherwise  = (ev, PBinaryOp PSequentialComp pn p2)

-- The transitions of a sliding choice are the transitions of p1 plus a tau
-- event which results in p2.
transitions o (PBinaryOp PSlidingChoice p1 p2) = (Tau, p2) :
  (map slideVisible $ transitions o p1)
  where slideVisible (Tau, pn) = (Tau, PBinaryOp PSlidingChoice pn p2)
        slideVisible x = x

-- The transitions of synchronising external choice are the transitions of each
-- member of ps, minus those with events in evs. Those in evs must be offered by
-- all ps and do not resolve the choice. Tau does not resolve the choice either.
transitions o (POp (PSynchronisingExternalChoice evs) ps) =
  foldr (insertBy (comparing fst)) frees syncs
  -- Frees - analogous to External Choice, but filtering out events in evs.
  where frees = filterFree (S.viewl evs') $ transitionsMap syncEC o ps
        filterFree _ [] = []
        filterFree S.EmptyL ts = ts
        filterFree (e S.:< es) ((en, pn):ts)
          | e < en = filterFree (S.viewl es) ((en, pn):ts)
          | e > en = (en, pn):(filterFree (e S.:< es) ts)
          | otherwise = filterFree (e S.:< es) ts
        syncEC n (Tau, pn) =
          (Tau, POp (PSynchronisingExternalChoice evs) (S.update n pn ps))
        syncEC _ (ev, pn) = (ev, pn)
  -- Analogous to Generalized Parallel, but here tick resolves the choice.
        syncs = synchronize
          (POp (PSynchronisingExternalChoice evs) ps) as tss (F.toList evs')
        evs' = S.unstableSort evs
        tss = fmap (transitions o) ps
        as = S.replicate (S.length ps) evs'

-- The transitions of synchronising interrupt are the transitions of the first
-- component with events not in the given list resulting in non-resolution of
-- the interrupt, plus those of the second component resulting in the resolution
-- of the interrupt, plus those with events offered by both components resulting
-- in the event occurring in both resulting in non-resolution, plus tau events
-- in the second component resulting in non-resolution.
transitions o (PBinaryOp (PSynchronisingInterrupt evs) p1 p2) =
  doInterrupt (S.viewl (S.unstableSort evs)) (transitions o p1) (transitions o p2)
  -- This is a kind of three way merge, iterating over the ordered (by event)
  -- lists of synchronising events, transitions of p1 and transitions of p2.
  where doInterrupt _ [] [] = []
  -- Tau action in the interrupting process.
        doInterrupt es ts1 ((Tau, p2'):ts2) =
          (Tau, PBinaryOp (PSynchronisingInterrupt evs) p1 p2'):
          (doInterrupt es ts1 ts2)
  -- No interrupting transitions left - pass everything not in es.
        doInterrupt S.EmptyL ((e1, p1'):ts1) [] =
          (e1, PBinaryOp (PSynchronisingInterrupt evs) p1' p2):
          (doInterrupt S.EmptyL ts1 [])
        doInterrupt (e S.:< es) ((e1, p1'):ts1) []
          | e < e1 = doInterrupt (S.viewl es) ((e1, p1'):ts1) []
          | e > e1 = (e1, PBinaryOp (PSynchronisingInterrupt evs) p1' p2):
              (doInterrupt (e S.:< es) ts1 [])
          | otherwise = doInterrupt (e S.:< es) ts1 []
  -- Nothing left in the first component - pass everything not in es. Special
  -- case for Tau has already been dealt with.
        doInterrupt S.EmptyL [] ((e2, p2'):ts2) = (e2, p2'):
          (doInterrupt S.EmptyL [] ts2)
        doInterrupt (e S.:< es) [] ((e2, p2'):ts2)
          | e < e2 = doInterrupt (S.viewl es) [] ((e2, p2'):ts2)
          | e > e2 = (e2, p2'):(doInterrupt (e S.:< es) [] ts2)
          | otherwise = doInterrupt (e S.:< es) [] ts2
  -- Something in both components, nothing left in es - behave as a normal
  -- interrupt.
        doInterrupt S.EmptyL ((e1, p1'):ts1) ((e2, p2'):ts2)
          | e1 <= e2 = (e1, PBinaryOp (PSynchronisingInterrupt evs) p1' p2):
              (doInterrupt S.EmptyL ts1 ((e2, p2'):ts2))
          | otherwise = (e2, p2'):(doInterrupt S.EmptyL ((e1, p1'):ts1) ts2)
  -- Something in all three lists. This is the big one.
        doInterrupt (e S.:< es) ((e1, p1'):ts1) ((e2, p2'):ts2)
          -- Current sync event has already been passed, move to the next.
          | e < e1 && e < e2 =
              doInterrupt (S.viewl es) ((e1, p1'):ts1) ((e2, p2'):ts2)
          -- Current sync event offered by both sides, offer that event and
          -- update both sides without resolving the interrupt.
          | e == e1 && e == e2 =
              (e, PBinaryOp (PSynchronisingInterrupt evs) p1' p2'):
                (doInterrupt (e S.:< es) ts1 ts2)
          -- Current sync event offered by exactly one side, don't offer that
          -- event.
          | e == e1 = doInterrupt (e S.:< es) ts1 ((e2, p2'):ts2)
          | e == e2 = doInterrupt (e S.:< es) ((e1, p1'):ts1) ts2
          -- Now e > e1 and e > e2, so the events being considered aren't in
          -- the synchronising set in these last two cases. Left side events
          -- first.
          | e1 <= e2 = (e1, PBinaryOp (PSynchronisingInterrupt evs) p1' p2):
              (doInterrupt (e S.:< es) ts1 ((e2, p2'):ts2))
          -- And finally right side events.
          | otherwise = (e2, p2'):(doInterrupt (e S.:< es) ((e1, p1'):ts1) ts2)

-- The transitions for a process call are simply those of the inner process
transitions o (PProcCall pn p) = transitions o p

tMerge :: [(Event, UProc)] -> [(Event, UProc)] -> [(Event, UProc)]
tMerge [] ys = ys
tMerge xs [] = xs
tMerge (x:xs) (y:ys)
  | comparing fst x y == GT = y:(tMerge (x:xs) ys)
  | otherwise               = x:(tMerge xs (y:ys))
