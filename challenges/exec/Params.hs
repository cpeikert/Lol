{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module Params where

import Control.Monad.Except
import Text.Parsec
import Text.Parsec.Token
import Generate

contLineID = "Cont"
discLineID = "Disc"
rlwrLineID = "RLWR"

lang :: (Stream s m Char) => GenLanguageDef s u m
lang = LanguageDef
  {commentStart = "/*",
   commentEnd = "*/",
   commentLine = "--",
   nestedComments = True,
   identStart = letter,
   identLetter = letter,
   opStart = letter,
   opLetter = letter,
   reservedNames = [],
   reservedOpNames = [],
   caseSensitive = True}

langParser :: (Stream s m Char) => GenTokenParser s u m
langParser = makeTokenParser lang

parseWhiteSpace :: (Stream s m Char) => ParsecT s u m ()
parseWhiteSpace = whiteSpace langParser

parseIntegral :: (Integral i, Monad m, Stream s m Char) => ParsecT s u m i
parseIntegral = (fromIntegral <$> natural langParser) <* parseWhiteSpace

parseDouble :: (Monad m, Stream s m Char) => ParsecT s u m Double
parseDouble = float langParser <* parseWhiteSpace

paramsFile :: (MonadError String m, Stream s m Char) => ParsecT s u m [ChallengeParams]
paramsFile = many line

line :: (MonadError String m, Stream s m Char) => ParsecT s u m ChallengeParams
line = rlwecParams <|> rlwedParams <|> rlwrParams <?> "Expected one of '" ++
  show contLineID ++ "', '" ++
  show discLineID ++ "', or '" ++
  show rlwrLineID ++ "'."

rlwecParams, rlwedParams, rlwrParams ::
  (MonadError String m, Stream s m Char) => ParsecT s u m ChallengeParams
rlwecParams = do
  try $ string contLineID
  whiteSpace langParser
  numSamples <- parseIntegral
  numInsts <- parseIntegral
  m <- parseIntegral
  q <- parseIntegral
  svar <- parseDouble
  eps <- parseDouble
  return Cont{..}

rlwedParams = do
  try $ string discLineID
  whiteSpace langParser
  numSamples <- parseIntegral
  numInsts <- parseIntegral
  m <- parseIntegral
  q <- parseIntegral
  svar <- parseDouble
  eps <- parseDouble
  return Disc{..}

rlwrParams = do
  try $ string rlwrLineID
  whiteSpace langParser
  numSamples <- parseIntegral
  numInsts <- parseIntegral
  m <- parseIntegral
  q <- parseIntegral
  p <- parseIntegral
  when (p > q) $ throwError $
    "Expected p <= q; parsed q=" ++ show q ++ " and p=" ++ show p
  return RLWR{..}

parseChallParams :: String -> [ChallengeParams]
parseChallParams input = do
  let output = runExcept $ runParserT paramsFile () "" input
  case output of
    Left e -> error $ "Invalid parameters:" ++ e
    Right r -> case r of
      Left e -> error $ "Error parsing input:" ++ show e
      Right r -> r
