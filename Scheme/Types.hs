{-# LANGUAGE TemplateHaskell #-}

module Scheme.Types where
import Data.DeriveTH
import Data.Maybe
import Data.IORef

import Control.Monad.Error

import qualified Data.Complex as C (Complex(..), realPart)
import Data.Ratio (Ratio, (%), numerator, denominator)
import Data.Vector as V (Vector, foldl)

import Text.Parsec.Error
import Text.Parsec.Pos

import System.IO
import DeriveGetMaybe

type ThrowsErrorIO = ErrorT LispError IO

data Env = Env
    { readOnly :: [(String, LispVal)]
    , definitions :: IORef [(String, IORef LispVal)] }
    deriving (Eq)

data LispVal
    = Atom String
    | Bool Bool
    | Num LispNum
    | Character Char
    | String String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Vector (V.Vector LispVal)
    | Func [String] (Maybe String) [LispVal] Env
    | PrimitiveFunc String
    | Port Handle
    deriving (Eq)

data LispNum
    = Int Integer
    | Rational (Ratio Integer)
    | Double Double
    | Complex (C.Complex Double)
    deriving (Show, Eq)

data LispError
    = ParserError ParseError
    | NumArgs Int [LispVal]
    | BadExpr String LispVal
    | TypeMismatch String String
    | UnboundVar String String
    | Default String

instance Show LispVal where
    show (Atom s)            = s
    show (Bool s)            = if s then "#t" else "#f"
    show (Num n)             = show n
    show (Character c)       = "\\#" ++ [c]
    show (String s)          = "\"" ++ s ++ "\""
    show (PrimitiveFunc f)   = "<primitive : " ++ f ++ ">"
    show (List vs)           = "(" ++ unwords (map show vs) ++ ")"
    show (DottedList vs dot) = "(" ++ unwords (map show vs)
                                   ++ " . " ++ show dot ++ ")"
    show (Vector vs)         = "#(" ++ V.foldl (\acc e -> show e ++ " " ++ acc) "" vs ++ ")"
    show (Func params vararg body _) =
        "(lambda (" ++ unwords params
        ++ maybe "" (" . "++) vararg ++ ") " ++ unwords (map show body)++ ")"
    show (Port h)            = "<port: " ++ show h ++ ">"

$( derive makeIs ''LispVal )
$( derive makeIs ''LispNum )
$( derive makeGet ''LispVal )

getInt :: LispNum -> Maybe Integer
getInt (Int n) = Just n
getInt _ = Nothing

getExact :: LispNum -> Maybe (Ratio Integer)
getExact (Int n) = Just (n % 1)
getExact (Rational n) = Just n
getExact _ = Nothing

getAsDouble :: LispNum -> Double
getAsDouble (Int n) = fromInteger n
getAsDouble (Rational n) = fromInteger (numerator n) / fromInteger (denominator n)
getAsDouble (Complex n) = C.realPart n
getAsDouble (Double n) = n

getAsComplex :: LispNum -> C.Complex Double
getAsComplex (Complex n) = n
getAsComplex n = getAsDouble n C.:+  0

---------AHHHHHHHHH!!!!!!!!
--the horror

--engulfs

instance Fractional LispNum where
    a / b = if isComplex a && isComplex b
            then Complex $ getAsComplex a / getAsComplex b
            else Double $ getAsDouble a / getAsDouble b
    recip a = (Complex $ 1 C.:+ 0) / a
    fromRational = Rational

instance Num LispNum where
    (Int a) + (Int b) = Int $ a + b
    (Rational a) + (Rational b) = Rational $ a + b
    (Double a) + (Double b) = Double $ a + b
    (Complex a) + (Complex  b) = Complex $ a + b
    a + b = uncurry (+) $ coerceNums a b

    (Int a) * (Int b) = Int $ a * b
    (Rational a) * (Rational b) = Rational $ a * b
    (Double a) * (Double b) = Double $ a * b
    (Complex a) * (Complex  b) = Complex $ a * b
    a * b = uncurry (*) $ coerceNums a b

    a - b = (\(a', b') -> a' + negate b') (coerceNums a b)
    abs (Int n) = Int           $ abs n
    abs (Rational n) = Rational $ abs n
    abs (Double n) = Double     $ abs n
    abs (Complex n) = Complex   $ abs n
    negate (Int n) = Int           $ negate n
    negate (Rational n) = Rational $ negate n
    negate (Double n) = Double     $ negate n
    negate (Complex n) = Complex   $ negate n
    signum (Int n) = Int           $ signum n
    signum (Rational n) = Rational $ signum n
    signum (Double n) = Double     $ signum n
    signum (Complex n) = Complex   $ signum n
    fromInteger = Int

coerceNums :: LispNum -> LispNum -> (LispNum, LispNum)
coerceNums al bl = fromJust $ msum
    [ do
         a <- getInt al
         b <- getInt bl
         return (Int a, Int b)
    , do
         a <- getExact al
         b <- getExact bl
         return (Rational a, Rational b)
    , if isComplex al && isComplex bl
        then Just (Complex $ getAsComplex al, Complex $ getAsComplex bl)
        else Nothing
    , Just (Double $ getAsDouble al, Double $ getAsDouble bl) ]


instance Show LispError where
         show (ParserError e)       = show e
         show (NumArgs n vs)        = "expected " ++ show n ++ " args, found " ++ show (length vs) ++ "\n\t" ++ unwords (map show vs)
         show (TypeMismatch want v) = "type mismatch. expected " ++ want ++ ", found " ++ show v
         show (BadExpr msg expr)    = msg ++ " : " ++ show expr
         show (UnboundVar msg var)  = msg ++ " : " ++ var
         show (Default msg)         = "error : " ++ msg

instance Error LispError where
    noMsg = Default "mysterious failure experienced."
    strMsg = Default

nullEnv readOnlys = newIORef [] >>= \e -> return (Env readOnlys e)

setVar :: Env -> String -> LispVal -> ThrowsErrorIO ()
setVar e@(Env readOnlys env) var lisp = liftIO (readIORef env) >>= \envList ->
       maybe (defineVar e var lisp)
             (const $ throwError $ Default "setting an existing variable") $ lookup var envList

getVar (Env readOnlys env) var = maybe findInDefinitions return (lookup var readOnlys)
    where findInDefinitions = liftIO (readIORef env) >>= maybe
                    (throwError $ Default $ "undefined atom \"" ++ var ++ "\"" )
                    (liftIO . readIORef) . lookup var

defineVar (Env readOnlys env) var lisp =
    do envList <- liftIO $ readIORef env
       maybe (return ()) (throwError . Default . ("redefining read-only "++) . show) (lookup var readOnlys)
       maybe (liftIO $ newIORef lisp >>= \wrapped -> writeIORef env ((var, wrapped):envList))
             (\oldLisp -> liftIO $ writeIORef oldLisp lisp) $ lookup var envList

showIOThrows :: (MonadIO m, Show e) => ThrowsErrorIO e -> m String
showIOThrows = (liftIO . runErrorT) >=> return . either show show


-- introspection!
errToLisp :: LispError -> LispVal
errToLisp (ParserError p) =
    List [Atom "parse-error"
        , List [Atom "source-name", String . sourceName $ errorPos p]
        , List [Atom "source-line", Num . Int . fromIntegral . sourceLine $ errorPos p]
        , List [Atom "source-column", Num . Int . fromIntegral . sourceColumn $ errorPos p]
        , List [Atom "message", String $ showErrorMessages "or" "unknown parse error"
                                    "expecting" "unexpected" "end of input"
                                    (errorMessages p)]]
errToLisp err = String . show $ err




