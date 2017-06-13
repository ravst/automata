{-# LANGUAGE FlexibleContexts #-}
module Automata.Regex.Parser(parseRegex, parseRegexInContext) where

import Text.Parsec
import Text.Parsec.Char

import Automata.Regex.Data

type RegexWithVars = Regex Char String


specialChars :: [Char]
specialChars = "|#\\()*"

parseAlternative :: Stream s m Char => ParsecT s u m ()
parseAlternative = char '|' >> return ()

parseEscapedChar :: Stream s m Char => ParsecT s u m Char
parseEscapedChar = char '\\' *> oneOf specialChars

parseChar :: Stream s m Char => ParsecT s u m Char
parseChar = noneOf specialChars <|> parseEscapedChar

parseRegex :: Stream s m Char => ParsecT s u m RegexWithVars
parseRegex = parseExp1 <* eof

parseExp1 :: Stream s m Char => ParsecT s u m RegexWithVars
parseExp1 = do
  l <- parseExp2
  ((Union l) <$> (parseAlternative *> parseExp1)) <|> (return l)

parseExp2 :: Stream s m Char => ParsecT s u m RegexWithVars
parseExp2 = do
  l <- parseExp2half
  ((Concat l) <$> parseExp2) <|> (return l)

parseExp2half :: Stream s m Char => ParsecT s u m RegexWithVars
parseExp2half = do
  l <- parseExp3
  (char '*' *> return (Iterate l)) <|> (return l)

parseExp3 :: Stream s m Char => ParsecT s u m RegexWithVars
parseExp3 = parseExp1InParens <|> (Letter <$> parseChar) <|> parseAutomaton
  where
    parseExp1InParens = char '(' *> parseExp1 <* char ')'
    parseAutomaton = char '#' *> (Feature <$> between (char '(') (char ')') (many letter))


-- based on https://wiki.haskell.org/Quasiquotation#The_Quasiquoter
parseRegexInContext :: Monad m => (String, Int, Int) -> String -> m RegexWithVars
parseRegexInContext (file, line, col) s =
    case runParser p () "" s of
      Left err  -> fail $ show err
      Right e   -> return e
  where
    p = do  pos <- getPosition
            setPosition $
              (flip setSourceName) file $
              (flip setSourceLine) line $
              (flip setSourceColumn) col $
              pos
            spaces
            e <- parseExp1
            eof
            return e
