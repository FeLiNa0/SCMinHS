module Scheme.Eval.Primitives (ioPrimitives, basicPrimitives) where

import Control.Monad
import Data.Maybe
import Data.Complex as C
import Data.Ratio
import Control.Monad.Error
import Scheme.Types
import System.IO

import Scheme.Eval.DynamicTypes



prim = undefined

convert typ wrap = expect typ >=> return . wrap

foldlOnType typ f = foldM (\acc e -> if check typ e
    then return (acc `f` e)
    else errorNeedType typ (show e))
foldlOnType1 typ f initPrep l = do expect typ (head l)
                                   foldlOnType typ f (initPrep $ head l) (init l)

basicPrimitives :: [(String, [LispVal] -> ThrowsErrorIO LispVal)]
basicPrimitives =
    let toBool = return . Bool
        dol = dottedOrPlainListType
    in
    -- number type checking
    [ ("number?",
        oneArgOnly >=> toBool . isNum)
    , ("real?",
        oneArgOnly >=> toBool . checkNum isDouble)
    , ("rational?",
        oneArgOnly >=> toBool . checkNum isRational)
    , ("integer?",
        oneArgOnly >=> toBool . checkNum isInt)
    , ("complex?",
        oneArgOnly >=> toBool . checkNum isComplex)

    -- only nice rationals and integers are exact
    , ("exact?",
        oneArgOnly >=> \n -> toBool $ any ($n) [checkNum isRational, checkNum isInt])

    -- arithmetic, preserve exactness in this order: integer, rational, float, complex
    , ("+",
        minArgs 2 >=> expect (listOf numType) . List >=> return . Num . sum)
    , ("-",
        minArgs 2 >=> expect (listOf numType) . List >=> return . Num . foldl1 (-))
    , ("*",
        minArgs 2 >=> expect (listOf numType) . List >=> return . Num . product)
    , ("/",
        onlyArgs 2 >=> \[_a, _b] -> do a <- expect numType _a
                                       b <- expect numType _b
                                       return . Num $ a / b)
    , ("negate",
        oneArgOnly >=> expect numType >=> return . Num . negate)
    , ("signum",
        oneArgOnly >=> expect numType >=> return . Num . signum)
    , ("abs",
        oneArgOnly >=> expect numType >=> return . Num . abs)
    , ("recip",
        oneArgOnly >=> expect numType >=> return . Num . recip)

    -- integer arithmetic functions
    , ("mod",
        minArgs 2 >=> foldlOnType1 numIntType (\(Num (Int a)) (Num (Int b))
                                              -> Num . Int $ a `mod` b) id)
    , ("quotient",
        minArgs 2 >=> foldlOnType1 numIntType (\(Num (Int a)) (Num (Int b))
                                              -> Num . Int $ a `quot` b) id)
    , ("remainder",
        minArgs 2 >=> foldlOnType1 numIntType (\(Num (Int a)) (Num (Int b))
                                              -> Num . Int $ a `rem` b) id)

    -- boolean
    , ("boolean?", oneArgOnly >=> toBool . isBool)
    , ("&&",
        minArgs 1 >=> liftM Bool . foldlOnType boolType (\a b -> a && fromJust (getBool b)) True)
    , ("||",
        minArgs 1 >=> liftM Bool . foldlOnType boolType (\a b -> a || fromJust (getBool b)) False)

    -- list processing !
    , ("list?",
        oneArgOnly >=> \l -> toBool $ any ($l) [isList, isDottedList, isVector])
    , ("dottedlist?",
        oneArgOnly >=> toBool . isDottedList)


    , ("car",
        oneArgOnly >=> expect dol >=> return .
                        (return . either id fst >=> safeHead)
                   >=> maybe (throwError $ Default "head-less list given to car") return)

    , ("cdr",
        oneArgOnly >=> expect dol >=> return .
                        --extract the dotted/normal list
                        either (liftM List . safeTail)
                            -- handle two cases for dotted lists
                            (\(xs, dot) -> Just $ if null xs then dot else DottedList (tail xs) dot)

                    -- handle bad tail
                   >=> maybe (throwError $ Default "tail-less list given to cdr") return)

    , ("cons", onlyArgs 2 >=> (\(a:b:[]) ->
                maybe (return $ List [a,b]) return
                    (get dol b >>= either (Just . List . (a:)) (\(xs, dot) -> Just $ DottedList (a:xs) dot))
                ))



    , ("vector?",
        oneArgOnly >=> toBool . isVector)
    , ("vector-ref", prim)

    , ("vector-set", prim)
    , ("list->vector",
        oneArgOnly >=> prim)
    , ("vector->list",
        oneArgOnly >=> prim)

    -- symbol processing
    , ("symbol?",
        oneArgOnly >=> toBool . isAtom)

    , ("string->symbol",
        oneArgOnly >=> convert stringType Atom)
    , ("symbol->string",
        oneArgOnly >=> convert atomType String)

    -- string processing
    , ("string?",
        oneArgOnly >=> toBool . isString)
    , ("string-ref",
        onlyArgs 2 >=> \([lispstr, lispix]) -> do
            str <- expect stringType lispstr
            ix  <- expect numIntType lispix
            char           <- maybe (throwError $ BadExpr "index out of bounds" lispix) return (safeIndex (fromIntegral ix) str)
            return . Character $ char)

    -- string construction
    , ("string->list",
        oneArgOnly >=> convert stringType (List . map Character))
    , ("list->string",
        convert (listOf charType) String . List)

    -- characters
    , ("character?",
        oneArgOnly >=> toBool . isCharacter)
    , ("character=", minArgs 1 >=> liftM (Bool . fst) . foldlOnType charType (\(res, prev) c -> (isNothing prev || getCharacter c == prev, getCharacter c)) (True, Nothing))

    -- equivalence
    , ("eq?",    onlyArgs 2 >=> return . eqv)
    , ("eqv?",   onlyArgs 2 >=> return . eqv)
    , ("=",     onlyArgs 2 >=> return . eqv)
    , ("equal?", onlyArgs 2 >=> return . equal)
    , ("==",    onlyArgs 2 >=> return . equal)
    ]

ioPrimitives =
    [ ("open-input-file", oneArgOnly >=> expect stringType >=> makePort ReadMode)
    , ("open-output-file", oneArgOnly >=> expect stringType >=> makePort WriteMode)
    , ("close-port", closePort)
    , ("close-input-port", closePort)
    , ("close-output-port", closePort)
    , ("read-contents", oneArgOnly >=> expect portType >=> liftM String . liftIO . hGetContents)
    , ("write", maxArgs 2 >=> minArgs 1 >=> \l -> case l of
        [obj] -> safeLiftIO (print obj) >> lispNull
        [obj, p] -> expect portType p >>= \port -> safeLiftIO (hPrint port (show obj)) >> lispNull)
    ]

closePort = oneArgOnly >=> liftIO . hClose . fromJust . getPort >=> const lispNull

makePort mode = liftM Port . safeLiftIO . flip openFile mode

eqv [a,b]
    | a == List [] && a == b = Bool True
    | all (\x -> check (dottedListType `orType` listType `orType` vectorType) x) [a,b] = Bool False
    | otherwise = equal [a,b]

equal [a,b] = Bool $ a == b



