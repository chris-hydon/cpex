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

transitionsMap f =
  F.concat . (S.mapWithIndex (\n p -> map (f n) (transitions p)))

--transitions (POp (PAlphaParallel evs) ps)

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

--transitions (PBinaryOp PInterrupt p1 p2)

--transitions (PBinaryOp (PLinkParallel evm) p1 p2)

--transitions (PUnaryOp (POperator ProcOperator) p)

-- The only transition here is to perform the event and move to p
transitions (PUnaryOp (PPrefix ev) p) = [(ev, p)]

--transitions (PUnaryOp (PRename evm) p)

--transitions (PBinaryOp PSequentialComp p1 p2)

--transitions (PBinaryOp PSlidingChoice p1 p2)

-- The transitions for a process call are simply those of the inner process
transitions (PProcCall pn p) = transitions p
