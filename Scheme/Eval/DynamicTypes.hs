module Scheme.Eval.DynamicTypes where

import Control.Monad
import Data.Maybe
import Control.Monad.Error

import Scheme.Types

safeIndex ix l = if ix < 0 || ix + 1 > length l then Nothing else Just $ l !! ix

safeHead (x:xs) = Just x
safeHead _ = Nothing

safeTail (x:xs) = Just xs
safeTail _ = Nothing

safeLiftIO :: IO a -> ThrowsErrorIO a
safeLiftIO action =
    liftIO (catchError
        (liftM Right action)
        (return . Left . ("io error\n"++) . show))
    >>= either (throwError . Default) return

maybeToBool :: Maybe Bool -> Bool
maybeToBool = fromMaybe False



data TypeInfo a = TInfo { get :: LispVal -> Maybe a, check :: LispVal -> Bool, name :: String}

infixr 3 `orType`
(TInfo get1 check1 name1) `orType` (TInfo get2 check2 name2) = TInfo
    (\v -> if isNothing $ get1 v then liftM Right $ get2 v else liftM Left $ get1 v)
    (\v -> check1 v || check2 v) (name1 ++ " or " ++ name2)


stringType  = TInfo getString isString "string"
portType  = TInfo getPort isPort "port"
charType    = TInfo getCharacter isCharacter "character"
boolType    = TInfo getBool isBool "boolean"
atomType    = TInfo getAtom isAtom "atom/symbol"
primType    = TInfo getPrimitiveFunc isPrimitiveFunc "primitive"
funcType    = TInfo getFunc isFunc "function"
numType     = TInfo getNum isNum "number"
numIntType  = TInfo (getNum >=> getInt) (checkNum isInt) "integer"

checkNum isthis = maybeToBool . liftM isthis . getNum

listType = TInfo getList isList "list"
dottedListType = TInfo getDottedList isDottedList "improper list"
vectorType  = TInfo getVector isVector "vector"

listOf :: TypeInfo a -> TypeInfo [a]
listOf elemType = TInfo
    (getList >=> return . map (get elemType) >=> sequence)
    (maybeToBool . (getList >=> return . all (check elemType)))
    ("list with elements of type " ++ name elemType)

dottedOrPlainListType = listType `orType` dottedListType



oneArgOnly ([a]) = return a
oneArgOnly args = throwError $ NumArgs 1 args

onlyArgs n args = if length args == n then return args
                 else throwError $ NumArgs n args

minArgs n args = if length args >= n then return args
                                     else throwError $ NumArgs n args

maxArgs n args = if length args <= n then return args
                                     else throwError $ NumArgs n args

expect typ v = if check typ v then return . fromJust $ get typ v else errorNeedType typ (show v)

errorNeedType typ s = throwError $ TypeMismatch (name typ) s

lispNull :: ThrowsErrorIO LispVal
lispNull = return $ List []



