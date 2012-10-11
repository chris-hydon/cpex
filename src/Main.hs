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
        liftIO $ putStrLn $ show $ pp expressionValue
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

pp :: UProc -> String
pp (POp (PAlphaParallel (evss)) ps) = foldr1 (\x y -> x ++ "; " ++ y)
    [foldr1 (\x y -> x ++ ", " ++ y) [render . prettyPrint $ ev | ev <- F.toList evs] |
     evs <- F.toList evss]
pp (POp (PGenParallel evs) ps) =
    foldr1 (\x y -> x ++ ", " ++ y) [render . prettyPrint $ ev | ev <- F.toList evs]
pp (POp (PLinkParallel evm) ps) =
    foldr1 (\x y -> x ++ ", " ++ y) [((render . prettyPrint) e1) ++ ((render . prettyPrint) e2) | (e1,e2) <- F.toList evm]
pp _ = "?"

-- | Given a process, returns a list of transitions that the process has,
-- represented as a pair of (event, resulting process)
transitions :: UProc -> [(Event, UProc)]
transitions' :: UProc -> [Event] -> [(Event, UProc)]
-- Given an event and a list of events, return the event if it is not in the
-- list, otherwise return Tau.
hide :: Event -> [Event] -> Event
-- Given a sequence and two values, return a sequence with the first value
-- replaced by the second, the first time it is encountered.
replace :: Eq a => S.Seq a -> a -> a -> S.Seq a

hide ev hidden = if elem ev hidden then Tau else ev

replace xs x xn = update (S.elemIndexL x xs) xn xs
    where update Nothing _ xs = xs
          update (Just i) xn xs = S.update i xn xs

-- The transitions for a process call are simply those of the inner process
transitions' (PProcCall pn p) hidden = transitions' p hidden
-- The only transition here is to perform the event and move to p
transitions' (PUnaryOp (PPrefix ev) p) hidden = [(hide ev hidden, p)]
-- The transitions for external choice are to perform some event and move to the
-- resulting process, and the processes resulting from each tau event.
transitions' (POp PExternalChoice ps) hidden =
    concatMap externalchoice (F.toList ps)
    where externalchoice p = map (withTau p) $ transitions' p hidden
          -- Tau event: keep the other external choices.
          withTau p (Tau, pn) = (Tau, POp PExternalChoice (replace ps p pn))
          -- Other event: discard the other choices.
          withTau _ (ev, pn) = (hide ev hidden, pn)
-- The transitions for internal choice are to perform a tau event for each
-- branch.
transitions' (POp PInternalChoice ps) hidden = [(Tau, p) | p <- (F.toList ps)]
-- The transitions for a process with hidden events are the transitions of that
-- process, with the hidden events replaced by tau.
--transitions' (PUnaryOp (PHide evs) p) hidden =
--    map (\(ev, p) -> (ev, PUnaryOp (PHide evs) p)) $ transitions' p $ hidden ++ (F.toList evs)
transitions' (PUnaryOp (PHide evs) p) hidden =
    map (\(ev, p) -> (if elem ev (F.toList evs) then Tau else ev, PUnaryOp (PHide evs) p)) $ transitions' p hidden
-- The transitions for an interleaved process are the transitions resulting from
-- the events offered by each process in parallel.
transitions' (POp PInterleave ps) hidden =
    concatMap (\p -> map (\(ev, pn) -> (ev, POp PInterleave (replace ps p pn))) (transitions' p hidden)) (F.toList ps)
-- The transitions for an alphabetized parallel process are the transitions
-- resulting from the events offered by each process in parallel, minus those
-- resulting from events on which we are synchronizing that at least one process
-- does not offer.
--transitions' (POp (PAlphaParallel (evs)) ps) hidden =
--    [(ev, pn) | p <- F.toList ps, (e, pn) <- (transitions' p hidden), ev <- F.toList evs]

transitions p = transitions' p []
