module Scheme1 (main, parsePrint) where
import Text.Parsec
import Control.Monad
import Data.Ratio

main = mapM_ (putStrLn . parsePrint)
    [ "loop", "'(eek)", "'#(a b)", "'(a . d)"
    , "()", "(   a )", "(a)", "(    a    b  )", "(a b)"
    , "#()", "#(   a )", "#(a)", "#(    a    b  )", "#(a b)"
    , "(    a   b     .  c )", "(a b . c)", "(     . d)", "(. d)"]

parsePrint :: String -> String
parsePrint inp = case runParser lispParser "" inp inp of
    Left err -> "No match: " ++ show err
    Right val -> "Found value in " ++ inp ++ "\n" ++ show val




------ Section 1: Representation




data LispVal = Atom String
             | Bool Bool
             | Num LispNum
             | String String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | LispVector [LispVal]

data LispNum = Int Integer
             | Rational (Ratio Integer)
             | Float Double
             | Complex LispNum LispNum
             deriving Show

instance Show LispVal where
    show (Atom s)            = s
    show (String s)          = show s
    show (Num n)             = show n
    show (Bool s)            = if s then "#t" else "#f"
    show (List vs)           = "(" ++ unwords (map show vs) ++ ")"
    show (DottedList vs dot) = "(" ++ unwords (map show vs)
                                   ++ " . " ++ show dot ++ ")"
    show (LispVector vs)     = "#" ++ show (List vs)



------ Section 2: Parser




-- Stream type String
-- User state type String
type Parser a = Parsec String String a

lispParser :: Parser LispVal
lispParser = lispVal

lispVal = lispSequence <|> atom <|> num <|> stringLisp <|> quoted

spaces1 :: Parser ()
spaces1 = skipMany1 space

symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

atom = do first <- letter <|> symbol
          rest  <- many (letter <|> symbol <|> digit)
          return . Atom $ first:rest

num = parserZero

stringLisp :: Parser LispVal
stringLisp = do char '"'
                s <- many (noneOf "\"")
                char '"'
                return $ String s

quoted = do char '\''
            v <- lispVal
            return $ List [Atom "quote", v]

lispSequence = do
    isVector <-  (string "("  >> return False)
             <|> (string "#(" >> return True)
    spaces
    vs <- sepEndBy lispVal spaces1
    maybeDot <- optionMaybe (spaces >> char '.' >> spaces >> lispVal)
    spaces
    char ')'
    case maybeDot of
        Nothing  -> return $ if isVector then LispVector vs else List vs
        Just dot -> if isVector then unexpected "dotted vector!" else return (DottedList vs dot)

--_sepBy p sep = _sepBy1 p sep <|> return []
--
--_sepBy1 p sep = do
--    x <- p
--    xs <- manyTill (sep >> p) (stop)
--    return (x:xs)

toVector = head



------ Section 3: Evaluator




