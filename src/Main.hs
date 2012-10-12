module Main (
    main
) where

-- Some funky monad stuff (liftIO)
import Control.Monad.Trans
-- for the main CSPM functions
import CSPM
-- Sequences are alternative representations of lists that concatonate nicely.
-- They are used in CSPM in some places, but you can always convert them to
-- lists by calling "F.toList seq".
-- This generalises some of the haskell list functions to work over sequences.
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Set as Set
-- for mapMaybe, fromJust and isNothing
import Data.Maybe
-- for getArgs (command line arguments)
import System.Environment
-- for error handling on CSPM files
import Util.Exception
-- for pretty printing functions
import Util.PrettyPrint

main :: IO ()
main = do
    filename:expression:_ <- getArgs
    session <- newCSPMSession
    (maybeError, resultingSession) <- unCSPM session $ tryM $ do
        -- Parse the file, returning something of type PModule.
        parsedFile <- parseFile filename
        -- Rename the file, returning something of type TCModule.
        renamedFile <- renameFile parsedFile
        -- Typecheck the file, annotating it with types.
        typeCheckedFile <- typeCheckFile renamedFile
        -- Desugar the file, returning the version ready for evaluation.
        desugaredFile <- desugarFile typeCheckedFile
        -- Bind the file, making all functions and patterns available.
        bindFile desugaredFile

        -- The file is now ready for use, so now we build the expression
        -- to be evaluated. This will be evaluated in the context given by the
        -- above file.

        -- The following pattern is guaranteed to be correct as stringToValue
        -- checks that it is of type Proc
        VProc expressionValue <- stringToValue TProc expression

        -- Pretty print the value out to stdout
        liftIO $ putStrLn (show (prettyPrint expressionValue))
        liftIO $ putStrLn $ "The available transitions are"
        liftIO $ putStrLn $ show $ vcat $
            -- The operators <> <+> and vcat come from
            -- http://hackage.haskell.org/packages/archive/pretty/1.0.1.0/doc/html/Text-PrettyPrint-HughesPJ.html
            map (\ (ev, p) -> prettyPrint ev <> colon <+> prettyPrint p)
                (transitions expressionValue)

    case maybeError of
        Left e -> liftIO $ putStrLn $ show e
        Right _ -> liftIO $ putStrLn $ "Ok"
    return ()

-- | Given a process, returns a list of transitions that the process has,
-- represented as a pair of (event, resulting process)
transitions :: UProc -> [(Event, UProc)]

-- Given a function (Int -> (Event, UProc) -> (Event, UProc) and a sequence of
-- UProcs, finds all transitions available to each UProc in the sequence and
-- applies the function to each, returning a list of resulting (Event, UProc)
-- pairs.
transitionsMap :: (Int -> (Event, UProc) -> (Event, UProc)) -> S.Seq UProc ->
                  [(Event, UProc)]
-- As transitionsMap, but also takes a second function to filter the results.
-- This function also has access to the process index.
transitionsMapFilter :: (Int -> (Event, UProc) -> (Event, UProc)) ->
                        (Int -> (Event, UProc) -> Bool) -> S.Seq UProc ->
                        [(Event, UProc)]

transitionsMap f =
  transitionsMapFilter f (\_ _ -> True)
transitionsMapFilter f g =
  F.concat . (S.mapWithIndex (\n p -> filter (g n) (map (f n) (transitions p))))

-- The transitions for an alphabetized parallel process are the transitions
-- resulting from the events offered by each process in parallel, minus those
-- resulting from events on which we are synchronizing that at least one process
-- syncing on that event does not offer.
transitions (POp (PAlphaParallel as) ps) = frees ++ syncs
  -- All events involved in synchronization. Don't want duplication - use a set.
  where sevs = F.foldr (\a evs -> F.foldr Set.insert evs a) Set.empty as
  -- Transitions involving events not in the alphabet for any process.
        frees = transitionsMapFilter alphaPar freeFilter ps
        alphaPar n (ev, pn) = (ev, POp (PAlphaParallel as) (S.update n pn ps))
        freeFilter n (ev, _) =
          (Set.notMember ev sevs) || (F.notElem ev (S.index as n))
  -- Other events: allow only if all processes that synchronize on the event
  -- offer it. Map each event to Just the resulting transition, or Nothing if
  -- the event is blocked. To do this, we reduce from ps, updating each process
  -- as it shows up, turning to Nothing on error.
        syncs = mapMaybe (
            \ev -> F.foldr (reduce ev) (Just (ev, POp (PAlphaParallel as) ps))
              (S.zip3 (S.fromList [0..(S.length as)]) as ps)
          ) (Set.toList sevs)
  -- The reduce function checks that the given process/alphabet either ignores
  -- or provides the event. In the former case, the event does not modify the
  -- state of that process. In the latter, the event occurs within that process.
  -- If neither is true, the event is blocked so return Nothing.
        reduce _ _ Nothing = Nothing
        reduce ev (n, evs, p) (Just (e, POp op qs)) -- ev == e
          | F.notElem ev evs = Just (e, POp op qs)
          | isNothing lookup = Nothing
          | otherwise        = Just (e, POp op (S.update n (fromJust lookup) qs))
          where lookup = foldr (\(e, pn) x -> if e == ev then Just pn else x)
                  Nothing (transitions p)

--transitions (PBinaryOp (PException evs) p1 p2)

-- The transitions for external choice are to perform some event and move to the
-- resulting process, and the processes resulting from each tau event.
transitions (POp PExternalChoice ps) = transitionsMap fixTau ps
  where fixTau n (Tau, pn) = (Tau, POp PExternalChoice (S.update n pn ps))
        fixTau _ (ev, pn) = (ev, pn)

--transitions (POp (PGenParallel evs) ps)

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
-- the events offered by each process in parallel.
transitions (POp PInterleave ps) =
  transitionsMap (\n (ev, pn) -> (ev, POp PInterleave (S.update n pn ps))) ps

-- The transitions for an interrupt are the events offered by p1 plus those
-- offered by p2, with the processes resulting from an event in p1 replaced by
-- an equivalent process interruptable by p2.
transitions (PBinaryOp PInterrupt p1 p2) =
  (map (\(ev, pn) -> (ev, PBinaryOp PInterrupt pn p2)) (transitions p1)) ++
  transitions p2

--transitions (PBinaryOp (PLinkParallel evm) p1 p2)

--transitions (PUnaryOp (POperator ProcOperator) p)

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
-- except Tick, which results in the process p2.
transitions (PBinaryOp PSequentialComp p1 p2) = map nextProcess $ transitions p1
  where nextProcess (ev, pn)
          | ev == Tick = (ev, p2)
          | otherwise  = (ev, PBinaryOp PSequentialComp pn p2)

--transitions (PBinaryOp PSlidingChoice p1 p2)

-- The transitions for a process call are simply those of the inner process
transitions (PProcCall pn p) = transitions p
