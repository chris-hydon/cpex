module Main (
    main
) where

-- Some funky monad stuff (liftIO)
import Control.Monad.Trans
-- For transitions function
import Cpex.Transitions
-- for the main CSPM functions
import CSPM
-- Sequences are alternative representations of lists that concatonate nicely.
-- They are used in CSPM in some places, but you can always convert them to
-- lists by calling "F.toList seq".
-- This generalises some of the haskell list functions to work over sequences.
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Set as Set
-- for partition
import Data.List
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
