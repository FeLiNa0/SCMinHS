module Main (main) where


import System.IO
import System.CPUTime
import System.IO.Error
import System.Environment
import System.Console.Haskeline

import Control.Monad.IO.Class
import Data.List (isPrefixOf)
import Data.IORef (readIORef)

import Scheme.Eval
import Scheme.Types (Env(..))


main :: IO ()
main =
    do
        hSetBuffering stdin NoBuffering
        as <- getArgs
        env <- primitiveEnv
        case length as of
            0 -> do putStrLn (helpStr ++ "\n\n" ++ creditsStr)
                    runInputT (haskelineSettings env) (loop (False, env))
            _ -> mapM_ (evalFile $ readEvalLines True) as
     where
        loop (timeIt, env) = do
             userSays <- getInputLine "lisp >>> "
             maybe
                (return ())
                (\expr ->
                    maybe
                    (maybeTime timeIt (readEvalPrint' expr env expr) >> loop (timeIt, env))
                    (\replFunc -> replFunc (timeIt, env) $ words expr)
                    (lookup (head $ words expr) replFuncs))
                userSays
        maybeTime rly action = if not rly then action else do
            a <- liftIO getCPUTime
            res <- action
            b <- liftIO getCPUTime
            outputStrLn $ "time: " ++ show (fromIntegral (b - a) / 1e12)
            return res
        replFuncs =
            [ (".?",    \e _ -> outputStrLn helpStr >> loop e)
            , (".help", \e _ -> printPrimitives >> loop e)
            , (".toggletime", \(time, env) _ -> do
                outputStrLn ("timing enabled: " ++ show (not time))
                loop (not time, env))
            , (".load", \e@(_, env) (_:fnames) -> do
                outputStrLn ("loading " ++ unwords fnames)
                mapM_ (evalFile $ readEvalLines' False env) fnames
                loop e)
            , (".quit", \_ _ -> outputStrLn "au revoir")
            ]
        lispCompletions (Env readOnlys bindingsIO) word = do
            bindings <- readIORef bindingsIO
            return $ filter (isPrefixOf word)
                    (map fst readOnlys ++ map fst bindings ++ map fst replFuncs)
                >>= \rep -> return Completion { replacement = rep, display = rep
                                              , isFinished = rep == word }
        helpStr = "Type a Lisp expression or a REPL command: " ++ unwords (map fst replFuncs)
        creditsStr = unlines
            [ "Written in VIM and compiled by Glasgow's Haskell Compiler"
            , "parsec, haskeline, base libraries, template haskell" ]
        printPrimitives = mapM_ (outputStrLn . fst) primitives
        evalFile evaluator fname = liftIO $
                catchIOError (readFile fname >>= evaluator fname)
                             (putStrLn . (("failure reading " ++ fname ++ "\n")++) . show)
        haskelineSettings env = flip setComplete defaultSettings $ \line -> do
            (_, cs1) <- completeWord Nothing " \t\n\r\f\v" (lispCompletions env) line
            (s, cs2) <- completeFilename line
            return (s, cs1 ++ cs2)


