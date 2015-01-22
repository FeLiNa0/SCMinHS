module Scheme.Eval
    ( eval, readEvalPrint, readEvalPrint', readEvalLines, readEvalLines'
    , primitiveEnv, primitives) where
import Control.Monad
import Data.Maybe
import Data.IORef
import Control.Monad.Error
import Scheme.Types
import System.IO

import Scheme.Parser
import Scheme.Eval.DynamicTypes
import Scheme.Eval.Primitives

type Continuation = LispVal

-- special primitives receive [LispVal] unevaluated,
-- also get full access to continuation and the environment
-- are responsible for calling eval if necessary
selfRefPrimitives :: [(String, Continuation -> Env -> [LispVal] -> ThrowsErrorIO LispVal)]
selfRefPrimitives =
    [ ("inspect", \cont env -> maxArgs 0 >=> const lispNull)
    , ("define", \_ env -> define env . List)
    , ("quote", \_ _ -> oneArgOnly)
    , ("if", \_ env -> onlyArgs 3 >=> \[maybecond,a,b] -> do
        cond <- eval env maybecond >>= expect boolType
        eval env $ if cond then a else b)
    , ("eval", \cont env -> oneArgOnly >=> \form -> case form of
        (List [Atom "quote",lisp]) -> eval env lisp
        lisp -> eval env lisp)
    , ("lambda", \_ env -> minArgs 2 >=> \(params:body) -> do
            params' <- expect (atomType `orType` listType `orType` dottedListType) params
            either
                (\atom -> makeVarargs (Atom atom) env [] body)
                (either (\ps -> makeNormalFunc env ps body)
                        (\(ps, varargs) -> makeVarargs varargs env ps body))
                params')
    , ("lambda-closure", \_ env -> oneArgOnly >=> eval env >=> expect funcType >=>
            \(ps, vars, bod, Env readOnly env) -> do
                e <- liftIO $ readIORef env
                liftM List $ mapM (\(n,vIO) ->
                    liftIO (readIORef vIO) >>= \v -> return $ List [String n, v]) e)
    , ("set!", \_ env -> onlyArgs 2 >=> \[a,b] -> do
            varName <- expect atomType a
            setVar env varName b
            lispNull)
    , ("load", \_ env -> oneArgOnly >=> expect (stringType `orType` portType)
        >=> either (\fname -> safeLiftIO $ readFile fname >>= evalAll env fname)
                   (\handle -> safeLiftIO $ hGetContents handle >>= evalAll env (show handle)))
    ]

-- when redefining, try to free any overwritten ports
define env (List [Atom v, form]) = eval env form >>= defineVar env v >> lispNull
define env (List (List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var >> lispNull
define env (List (DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var >> lispNull
define _   (List xs) = throwError $ TypeMismatch
    "first to be an atom or a dotted list or a normal list" (show xs)


evalPrimitives =
    [ ("apply-safe", minArgs 1 >=> \(func:rest) -> apply func $ if length rest == 1 && isList (head rest)
                then fromJust $ getList (head rest) else rest)
    , ("read", maxArgs 1 >=> \l -> (case l of
            [] -> return stdin
            [p] -> expect portType p)
            >>= \port ->
            (safeLiftIO . hGetLine >=> liftIO . readExpr ("read from: " ++ show port)) port)
    , ("read-all", maxArgs 1 >=> \l -> (case l of
            [] -> return stdin
            [p] -> expect portType p)
            >>= \port ->
            (safeLiftIO . hGetContents >=> liftIO . readAllExpr ("read-all from: " ++ show port)) port)
    , ("read-string", oneArgOnly >=> expect stringType >=> readExpr "<string>")
    , ("read-string-all", oneArgOnly >=> expect stringType >=> readAllExpr "<string>")
    ]

readOnlyPrimitives = toPrimitiveFuncs selfRefPrimitives

toPrimitiveFuncs = map (\(name, _) -> (name, PrimitiveFunc name))
primitives = evalPrimitives ++ basicPrimitives ++ ioPrimitives

primitiveEnv = nullEnv readOnlyPrimitives >>= flip bindVars (toPrimitiveFuncs primitives)

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars (Env readOnlys env) vars = do
    e <- readIORef env
    newE <- liftM (++e) (mapM (\(n,v) -> newIORef v >>= \vRef -> return (n, vRef)) vars)
    liftM (Env readOnlys) $ newIORef newE

makeFunc vararg env params body = do
    expect (listOf atomType) (List params)
    maybe (return ()) (void . expect atomType) vararg
    return $ Func (map show params) (fmap show vararg) body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> ThrowsErrorIO LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> ThrowsErrorIO LispVal
makeVarargs = makeFunc . Just



evalExpr env fname s = liftIO $
    runErrorT (readLisp fname s >>= eval env) >>=
    return . either errToLisp id

evalAll env fname s = liftIO $
    runErrorT (readLisps fname s >>= mapM (eval env)) >>=
    return . either errToLisp List

readExpr fname s = liftIO $
    runErrorT (readLisp fname s) >>=
    return . either errToLisp id

readAllExpr fname s = liftIO $
    runErrorT (readLisps fname s) >>=
    return . either errToLisp List


readEvalLines output fname s = liftIO $ do
    env <- primitiveEnv
    parseResult <- runErrorT $ readLisps fname s
    either print
        (mapM_ (showIOThrows . eval env >=> (when output . putStrLn)))
        parseResult

readEvalLines' output env fname s = liftIO $ do
    parseResult <- runErrorT $ readLisps fname s
    either print
        (mapM_ (showIOThrows . eval env >=> (when output . putStrLn)))
        parseResult

readEvalPrint fname lisp = liftIO $ primitiveEnv >>= \env -> readEvalPrint' fname env lisp
readEvalPrint' fname env = showIOThrows . (readLisp fname >=> eval env) >=> putFlushLn





eval :: Env -> LispVal -> ThrowsErrorIO LispVal
eval _ v@(Bool _)                = return v
eval _ v@(Num _)                 = return v
eval _ v@(Character _)           = return v
eval _ v@(String _)              = return v
eval env v@(Atom a)              = getVar env a




eval env (List (func:args))   = do
    f <- eval env func
    eitherName <- expect (atomType `orType` primType `orType` funcType) f
    let name' = liftM (either id id) (get (atomType `orType` primType) f)
    maybe
        (mapM (eval env) args >>= apply f)
        (\evalFunc -> evalFunc undefined env args)
        (name' >>= \name -> lookup name selfRefPrimitives)

eval _ badExpr                   = throwError $ BadExpr "unrecognized form" badExpr


apply p@(PrimitiveFunc f) args =
    maybe (throwError $ BadExpr "unrecognized primitive" p) ($args) $
    lookup f primitives
apply (Func params varargs body closure) args =
    if length params /= length args && isNothing varargs
    then throwError $ NumArgs (length params) args
    else liftIO (bindVars closure $ zip params args)
        >>= bindVarArgs varargs >>= \env -> liftM last (mapM (eval env) body)
  where
    remainingArgs = drop (length params) args
    bindVarArgs arg env = maybe (return env) (\argName -> liftIO $ bindVars env [(argName, List remainingArgs)]) arg

apply f _ = throwError . Default $ "cannot apply " ++ show f ++ " as function"

putFlushLn msg = liftIO $ putStrLn msg >> hFlush stdout
