module Scheme.Parser (readLisp, readLisps) where
import Control.Monad
import Control.Monad.Error (throwError)

import Data.Char
import Data.Maybe
import Data.Complex
import Data.Ratio
import Data.Vector as V (fromList)

import Text.Parsec hiding (spaces)

import Scheme.Types

readLisp :: FilePath -> String -> ThrowsErrorIO LispVal
readLisp fname inp = case parse (spaces >> lispVal) fname inp of
    Left err  -> throwError $ ParserError err
    Right val -> return val

readLisps :: FilePath -> String -> ThrowsErrorIO [LispVal]
readLisps fname inp = case parse lispParser fname inp of
    Left err  -> throwError $ ParserError err
    Right val -> return val

type Parser a = Parsec String () a

lispParser = spaces >> sepEndBy lispVal spaces

lispVal =
    try lispSequence <|> try num    <|> try lispChar <|> try bool <|>
    try atom         <|> stringLisp <|> quoted

comment = char ';' >> manyTill anyChar (try (char '\n' <|> char '\r'))

ignored = void space <|> void comment

spaces = many ignored

spaces1 :: Parser ()
spaces1 = skipMany1 ignored

symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

bool = char '#' >> (string "t" <|> string "f")
                >>= (\s -> return $ case s of "t" -> Bool True
                                              "f" -> Bool False)

atom = do first <- letter <|> symbol
          rest  <- many (letter <|> symbol <|> digit)
          return . Atom $ first:rest

lispChar :: Parser LispVal
lispChar = do
    string "#\\"
    special <- optionMaybe $ choice (map (string . fst) specialCharLits)
    maybe (liftM Character anyChar) (\lit -> return (Character . fromJust $ lookup lit specialCharLits)) special
  where specialCharLits = [("newline", '\n'), ("space", ' '), ("tab", '\t'), ("return", '\r')]

num = do
    let radices = [('x', 16), ('d', 10), ('o', 8), ('t', 3), ('b', 2)]
    maybeBase <- optionMaybe (try (char '#' >> oneOf (map fst radices)))
    maybeSign <- optionMaybe (char '+' <|> char '-')
    let sign :: Num a => a -> a
        sign = fromMaybe id $ maybeSign >>= \c -> if c == '-' then Just negate else Nothing
        base = case maybeBase of
                    Nothing -> 10
                    Just s -> fromMaybe 10 (lookup s radices)
    real <- try (liftM (Double   . sign) (parseFloat base))
        <|> try (liftM (Rational . sign) (parseRational base))
        <|>      liftM (Int      . sign) (parseInteger base)
    maybeImaginary <- optionMaybe (try $ num >>= \n -> char 'i' >> return n)
    return $ maybe (Num real) (\(Num imag) ->
        Num $ Complex $ getAsDouble real :+ getAsDouble imag) maybeImaginary

digitCharacters = ['0'..'9'] ++ ['A'..'Z']

toDigit :: (Num a, Enum a) => Char -> a
toDigit c = case lookup (toUpper c) $ zip digitCharacters [0..] of
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
        convertStr numAsStr = snd $ foldr
            (\c (plc, val) -> (plc + 1, val + toDigit c * fromIntegral base ** plc))
            (if isDecimal then (-1) * fromIntegral (length numAsStr) else 0.0, 0.0) numAsStr
    in liftM convertStr $ many1 (satisfy (flip elem allowedDigits . toUpper))

escapables = [('t', '\t'), ('n', '\n'), ('r', '\r'), ('\\', '\\')]

stringLisp :: Parser LispVal
stringLisp = do char '"'
                s <- many (noneOf "\"\\" <|> (try $ do
                        char '\\'
                        esc <- anyChar
                        return $ fromMaybe esc $ lookup esc escapables))
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
    return $ maybe
        (if isVector then Vector (toVector vs) else List vs)
        (DottedList vs) maybeDot

toVector = V.fromList
