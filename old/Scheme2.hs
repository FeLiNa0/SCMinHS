-- add fast, mutable vectors
-- repair string literals
-- comments
-- make pretty. RED 100!!!!!!!!!1

module Scheme2 (main, parsePrint) where
import Control.Monad
import Data.Ratio
import Data.Char
import Data.Maybe
import Data.Complex

import Text.Parsec

main = readFile "test1.lisp" >>= \ws -> mapM_ (putStrLn . parsePrint) (init $ lines ws)

parsePrint :: String -> String
parsePrint inp = case parse lispParser inp inp of
    Left err -> "ERROR!\n" ++ show err
    Right val -> "Evaluated expression " ++ inp ++ "\n" ++ show val




------ Section 1: Representation




data LispVal = Atom String
             | Bool Bool
             | Num LispNum
             | LispChar Char
             | String String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | LispVector [LispVal]

data LispNum = Int Integer
             | Rational (Ratio Integer)
             | Float Double
             | LispComplex (Complex Double)
             deriving Show

instance Show LispVal where
    show (Atom s)            = s
    show (Bool s)            = if s then "#t" else "#f"
    show (Num n)             = show n
    show (LispChar c)        = "character: '" ++ [c] ++ "'"
    show (String s)          = "string: " ++ show s
    show (List vs)           = "(" ++ unwords (map show vs) ++ ")"
    show (DottedList vs dot) = "(" ++ unwords (map show vs)
                                   ++ " . " ++ show dot ++ ")"
    show (LispVector vs)     = "#" ++ show (List vs)



------ Section 2: Parser




-- Stream type String
-- User state type ()
type Parser a = Parsec String () a

lispParser :: Parser LispVal
lispParser = lispVal

lispVal = try lispSequence <|> try num    <|> try lispChar
       <|> try atom    <|> stringLisp <|> quoted

spaces1 :: Parser ()
spaces1 = skipMany1 space

symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

atom = do first <- letter <|> symbol
          rest  <- many (letter <|> symbol <|> digit)
          return . Atom $ first:rest

lispChar :: Parser LispVal
lispChar = do
    string "#\\"
    special <- optionMaybe $ choice (map (string . fst) specialCharLits)
    maybe (liftM LispChar anyChar) (\lit -> return (LispChar . fromJust $ lookup lit specialCharLits)) special
  where specialCharLits = [("newline", '\n'), ("space", ' '), ("tab", '\t'), ("return", '\r')]

num = do
    maybeBase <- optionMaybe (try
            (char '#' >> (char 'b' <|> char 'o' <|> char 'd' <|> char 'x')))
    maybeSign <- optionMaybe (char '+' <|> char '-')
    let sign :: Num a => a -> a
        sign = fromMaybe id $ maybeSign >>= \c -> if c == '-' then Just negate else Nothing
    let base = case maybeBase of
                    Nothing   -> 10
                    Just 'b' -> 2
                    Just 'o' -> 8
                    Just 'd' -> 10
                    Just 'x' -> 16
                    _         -> 10
    real <- try (liftM (Float    . sign) (parseFloat base))
            <|>
            try (liftM (Rational . sign) (parseRational base))
            <|>
                 liftM (Int      . sign) (parseInteger base)
    maybeImaginary <- optionMaybe (try $ num >>= \n -> char 'i' >> return n)
    return $ maybe (Num real)
        (\(Num imag) -> Num . LispComplex $ coerceFloat real
                               :+ coerceFloat imag) maybeImaginary

  where int = parseInteger

coerceFloat (Int n) = fromIntegral n
coerceFloat (Float n) = n
coerceFloat (Rational n) = fromIntegral (numerator n) / fromIntegral (denominator n)
coerceFloat (LispComplex n) = realPart n

digitCharacters = ['0'..'9'] ++ ['A'..'Z']

toDigit :: (Num a, Enum a) => Char -> a
toDigit c = case lookup c $ zip digitCharacters [0..] of
    Nothing -> error $ "character not between 0 and 9 or A and Z: '" ++ [c] ++ "'"
    Just d -> d

parseInteger :: Int -> Parser Integer
parseInteger base = liftM round $ parseDigits False base

parseFloat :: Int -> Parser Double
parseFloat base = do big <- parseDigits False base
                     char '.'
                     small <- parseDigits True base
                     return $ big + small

parseRational :: Int -> Parser (Ratio Integer)
parseRational base = do n <- parseInteger base
                        char '/'
                        d <- parseInteger base
                        return $ n % d

parseDigits :: (Floating a, Enum a) => Bool -> Int -> Parser a
parseDigits isDecimal base =
    let allowedDigits = take base digitCharacters
    in liftM
         (\numAsStr -> fst $ foldr (\c (value, place) ->
                         ( value + toDigit (toUpper c) * fromIntegral base ** place, place + 1)
                      ) (0.0, if isDecimal then (-1) * fromIntegral (length numAsStr) else 0.0) numAsStr)
     $ many1 (satisfy (flip elem allowedDigits . toUpper))

escapables = [('t', '\t'), ('n', '\n'), ('r', '\r'), ('\\', '\\')]

stringLisp :: Parser LispVal
stringLisp = do char '"'
                s <- many (noneOf "\"" <|>
        ------------- FIXME
                    (try (char '\\' >> anyChar >>= \escaped -> return $ fromMaybe escaped (lookup escaped escapables))))
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
    maybeDot <- optionMaybe (spaces >> char '.' >> spaces1 >> lispVal)
    spaces
    char ')'
    case maybeDot of
        Nothing  -> return $ if isVector
            then LispVector (toVector vs) else List vs
        Just dot -> if isVector
            then unexpected "dotted vector!" else return (DottedList vs dot)

toVector = id



------ Section 3: Evaluator




