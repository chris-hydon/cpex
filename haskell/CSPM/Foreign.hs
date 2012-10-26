{-# LANGUAGE FlexibleInstances, ForeignFunctionInterface, ScopedTypeVariables,
    TypeSynonymInstances #-}
module CSPM.Foreign (SessionPtr, runSession) where

import Control.Monad.State.Strict
import CSPM
import Data.IORef
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Prelude hiding (catch)
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

cINTERACTIVE_STATEMENT_EXPRESSION :: CUInt
cINTERACTIVE_STATEMENT_EXPRESSION = 1

cINTERACTIVE_STATEMENT_BIND :: CUInt
cINTERACTIVE_STATEMENT_BIND = 2

cINTERACTIVE_STATEMENT_ASSERTION :: CUInt
cINTERACTIVE_STATEMENT_ASSERTION = 3

type FilePtr = StablePtr (CSPMFile Name)
type InteractiveStmtPtr = StablePtr (InteractiveStmt Name)
type SessionPtr = StablePtr (IORef NativeSession)

-- The actual foreign exports
foreign export ccall
    cspm_session_create :: IO SessionPtr
foreign export ccall
    cspm_session_free :: SessionPtr -> IO ()
foreign export ccall 
    cspm_session_get_errors :: SessionPtr -> Ptr (Ptr CWString) -> Ptr CUInt -> IO ()
foreign export ccall 
    cspm_session_get_warnings :: SessionPtr -> Ptr (Ptr CWString) -> Ptr CUInt -> IO ()
foreign export ccall 
    cspm_session_load_file :: SessionPtr -> CWString -> Ptr FilePtr -> IO CUInt
foreign export ccall 
    cspm_session_bound_names :: SessionPtr -> Ptr (Ptr CWString) -> Ptr CUInt -> IO CUInt
foreign export ccall
    cspm_session_parse_interactive_stmt :: SessionPtr -> CWString -> 
        Ptr InteractiveStmtPtr -> IO CUInt
foreign export ccall
    cspm_session_expression_type :: SessionPtr -> CWString -> Ptr CWString -> IO CUInt
foreign export ccall
    cspm_session_clear_warnings :: SessionPtr -> IO ()
foreign export ccall
    cspm_session_clear_errors :: SessionPtr -> IO ()

-- Interactive Statement
foreign export ccall
    cspm_interactive_stmt_free :: InteractiveStmtPtr -> IO ()
foreign export ccall
    cspm_interactive_stmt_type :: SessionPtr -> InteractiveStmtPtr -> 
        Ptr CUInt -> IO ()
foreign export ccall
    cspm_interactive_stmt_evaluate :: SessionPtr -> InteractiveStmtPtr -> 
        Ptr CWString -> IO CUInt
foreign export ccall
    cspm_interactive_stmt_bind :: SessionPtr -> InteractiveStmtPtr -> IO CUInt

-- Files
foreign export ccall
    cspm_file_free :: FilePtr -> IO ()

cspm_session_create :: IO SessionPtr
cspm_session_create = do
    sess <- newNativeSession
    ioref <- newIORef sess
    newStablePtr ioref

cspm_session_free :: SessionPtr -> IO ()
cspm_session_free sessPtr = freeStablePtr sessPtr

cspm_session_get_errors :: SessionPtr -> Ptr (Ptr CWString) -> Ptr CUInt -> IO ()
cspm_session_get_errors sess outArray outCount = runSessionHardErrors sess $ do
    es <- gets lastErrors
    liftIO $ do
        cwstrs <- mapM (newCWString . show . prettyPrint) es
        arrayPtr <- newArray cwstrs
        poke outArray arrayPtr
        poke outCount $ fromIntegral $ length es

cspm_session_get_warnings :: SessionPtr -> Ptr (Ptr CWString) -> Ptr CUInt -> IO ()
cspm_session_get_warnings sess outArray outCount = runSessionHardErrors sess $ do
    ws <- gets lastWarnings
    liftIO $ do
        cwstrs <- mapM (newCWString . show . prettyPrint) ws
        arrayPtr <- newArray cwstrs
        poke outArray arrayPtr
        poke outCount $ fromIntegral $ length ws

cspm_session_load_file :: SessionPtr -> CWString -> Ptr FilePtr -> IO CUInt
cspm_session_load_file sessPtr filePtr outPtr = runSession sessPtr $ do
    fp <- liftIO $ peekCWString filePtr
    parsedFile <- parseFile fp
    renamedFile <- renameFile parsedFile
    typeCheckedFile <- typeCheckFile renamedFile
    desugaredFile <- desugarFile typeCheckedFile
    bindFile desugaredFile
    ptr <- liftIO $ newStablePtr (unAnnotate desugaredFile)
    liftIO $ poke outPtr ptr

cspm_session_bound_names :: SessionPtr -> Ptr (Ptr CWString) -> Ptr CUInt -> IO CUInt
cspm_session_bound_names sessPtr outArray outCount = runSession sessPtr $ do
    ns <- getBoundNames
    cwstrs <- liftIO $ mapM (newCWString . show . prettyPrint) ns
    arrayPtr <- liftIO $ newArray cwstrs
    liftIO $ poke outArray arrayPtr
    liftIO $ poke outCount $ fromIntegral $ length cwstrs

cspm_session_expression_type :: SessionPtr -> CWString -> Ptr CWString -> IO CUInt
cspm_session_expression_type sessPtr expr outStr = runSession sessPtr $ do
    exprStr <- liftIO $ peekCWString expr
    expr <- parseExpression exprStr
    renamedExpr <- renameExpression expr
    typ <- typeOfExpression renamedExpr
    sptr <- liftIO $ newCWString (show (prettyPrint typ))
    liftIO $ poke outStr sptr

cspm_session_parse_interactive_stmt :: SessionPtr -> CWString -> 
    Ptr InteractiveStmtPtr -> IO CUInt
cspm_session_parse_interactive_stmt sessPtr stmt outPtr = runSession sessPtr $ do
    stmtStr <- liftIO $ peekCWString stmt
    stmt <- parseInteractiveStmt stmtStr
    renamedStmt <- renameInteractiveStmt stmt
    typeCheckedStmt <- typeCheckInteractiveStmt renamedStmt
    dsStmt <- desugarInteractiveStmt typeCheckedStmt
    ptr <- liftIO $ newStablePtr (unAnnotate dsStmt)
    liftIO $ poke outPtr ptr

cspm_session_clear_warnings :: SessionPtr -> IO ()
cspm_session_clear_warnings sessPtr = runSessionHardErrors sessPtr $
    modify (\st -> st { lastWarnings = [] })

cspm_session_clear_errors :: SessionPtr -> IO ()
cspm_session_clear_errors sessPtr = runSessionHardErrors sessPtr $
    modify (\st -> st { lastErrors = [] })

cspm_interactive_stmt_type :: SessionPtr -> InteractiveStmtPtr -> Ptr CUInt -> IO ()
cspm_interactive_stmt_type sessPtr istmtPtr outType = runSessionHardErrors sessPtr $ do
    stmt <- liftIO $ deRefStablePtr istmtPtr
    liftIO $ poke outType $ case stmt of
                                Bind _ -> cINTERACTIVE_STATEMENT_BIND
                                Evaluate _ -> cINTERACTIVE_STATEMENT_EXPRESSION
                                RunAssertion _ -> cINTERACTIVE_STATEMENT_ASSERTION
    return ()

cspm_interactive_stmt_free :: InteractiveStmtPtr -> IO ()
cspm_interactive_stmt_free istmtPtr = freeStablePtr istmtPtr

cspm_interactive_stmt_evaluate :: SessionPtr -> InteractiveStmtPtr -> 
    Ptr CWString -> IO CUInt
cspm_interactive_stmt_evaluate sessPtr istmtPtr outPtr = runSession sessPtr $ do
    stmt <- liftIO $ deRefStablePtr istmtPtr
    case stmt of
        Evaluate e -> do
            v <- evaluateExpression e
            sptr <- liftIO $ newCWString $ show $ prettyPrint v
            liftIO $ poke outPtr sptr
        _ -> panic "Incorrect interactive statement type"

cspm_interactive_stmt_bind :: SessionPtr -> InteractiveStmtPtr -> IO CUInt
cspm_interactive_stmt_bind sessPtr istmtPtr = runSession sessPtr $ do
    stmt <- liftIO $ deRefStablePtr istmtPtr
    case stmt of
        Bind ds -> mapM_ bindDeclaration ds
        _ -> panic "Incorrect interactive statement type"

cspm_file_free :: FilePtr -> IO ()
cspm_file_free fptr = freeStablePtr fptr

reportError :: NativeSessionMonad () -> NativeSessionMonad CUInt
reportError prog = do
    v <- tryM prog
    case v of
        Left err -> do
            let errs = case err of
                        Panic s -> [mkErrorMessage Unknown (text s)]
                        SourceError ms -> ms
                        UserError -> [mkErrorMessage Unknown (text "Unknown error")]
            modify (\s -> s { lastErrors = errs })
            return 0
        Right _ -> return 1

runSessionNoErrors :: SessionPtr -> NativeSessionMonad a -> IO a
runSessionNoErrors sessPtr prog = do
    sessRef <- deRefStablePtr sessPtr
    sess <- readIORef sessRef
    (a, sess') <- runStateT prog sess
    writeIORef sessRef sess'
    return a

runSession :: SessionPtr -> NativeSessionMonad () -> IO CUInt
runSession sessPtr prog = runSessionNoErrors sessPtr $ reportError prog

runSessionHardErrors :: SessionPtr -> NativeSessionMonad a -> IO a
runSessionHardErrors sessPtr prog = runSessionNoErrors sessPtr $ failOnError prog

foreign import ccall "stdlib.h abort" c_abort :: IO ()

failOnError :: NativeSessionMonad a -> NativeSessionMonad a
failOnError prog = do
    v <- tryM prog
    case v of
        Left v -> liftIO $ do
            putStrLn "A function that was supposed to never throw an error threw the following error: "
            putStrLn $ show v
            putStrLn "Terminating"
            c_abort
            error "Abort did not work"
        Right v -> return v

-- The Special Monad implementation
type NativeSessionMonad = StateT NativeSession IO

newNativeSession :: MonadIO m => m NativeSession
newNativeSession = do
    ms <- newCSPMSession
    return $! NativeSession [] [] ms

data NativeSession = NativeSession {
        lastErrors :: [ErrorMessage],
        lastWarnings :: [ErrorMessage],
        mainSession :: CSPMSession
    }

instance CSPMMonad NativeSessionMonad where
    getSession = gets mainSession
    setSession ms = modify (\s -> s { mainSession = ms })
    handleWarnings ws = modify (\ s -> s { lastWarnings = ws })
