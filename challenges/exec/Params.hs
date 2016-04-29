{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module Params where

import Common (InstanceID)

import Control.Monad.Except
import Data.Int
import Text.Parsec
import Text.Parsec.Token

-- | Information to generate a challenge.
data ChallengeParams =
    C {numSamples::Int, numInsts::InstanceID, m::Int32, q::Int64,
       svar::Double, eps::Double}
  | D {numSamples::Int, numInsts::InstanceID, m::Int32, q::Int64,
       svar::Double, eps::Double}
  | R {numSamples::Int, numInsts::InstanceID, m::Int32, q::Int64, p::Int64}
  deriving (Show)

contLineID, discLineID, rlwrLineID :: String
contLineID = "Cont"
discLineID = "Disc"
rlwrLineID = "RLWR"

-- default probability eps to use
epsDef :: Double
epsDef = 2 ** (-40)

lang :: (Stream s m Char) => GenLanguageDef s InstanceID m
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

langParser :: (Stream s m Char) => GenTokenParser s InstanceID m
langParser = makeTokenParser lang

parseWhiteSpace :: (Stream s m Char) => ParsecT s InstanceID m ()
parseWhiteSpace = whiteSpace langParser

parseIntegral :: (Integral i, Monad m, Stream s m Char) => ParsecT s InstanceID m i
parseIntegral = (fromIntegral <$> natural langParser) <* parseWhiteSpace

parseDouble :: (Monad m, Stream s m Char) => ParsecT s InstanceID m Double
parseDouble = float langParser <* parseWhiteSpace

paramsFile :: (MonadError String m, Stream s m Char) => ParsecT s InstanceID m [ChallengeParams]
paramsFile = many line

line :: (MonadError String m, Stream s m Char) => ParsecT s InstanceID m ChallengeParams
line = rlwecParams <|> rlwedParams <|> rlwrParams <?> "Expected one of '" ++
  show contLineID ++ "', '" ++
  show discLineID ++ "', or '" ++
  show rlwrLineID ++ "'."

rlwecParams, rlwedParams, rlwrParams ::
  (MonadError String m, Stream s m Char) => ParsecT s InstanceID m ChallengeParams
rlwecParams = do
  _ <- try $ string contLineID
  whiteSpace langParser
  numSamples <- parseIntegral
  numInsts <- getState
  m <- parseIntegral
  q <- parseIntegral
  svar <- parseDouble
  let eps = epsDef
  return C{..}

rlwedParams = do
  _ <- try $ string discLineID
  whiteSpace langParser
  numSamples <- parseIntegral
  numInsts <- getState
  m <- parseIntegral
  q <- parseIntegral
  svar <- parseDouble
  let eps = epsDef
  return D{..}

rlwrParams = do
  _ <- try $ string rlwrLineID
  whiteSpace langParser
  numSamples <- parseIntegral
  numInsts <- getState
  m <- parseIntegral
  q <- parseIntegral
  p <- parseIntegral
  when (p > q) $ throwError $
    "Expected p <= q; parsed q=" ++ show q ++ " and p=" ++ show p
  return R{..}

parseChallParams :: String -> InstanceID -> [ChallengeParams]
parseChallParams input numInsts = do
  let output = runExcept $ runParserT paramsFile numInsts "" input
  case output of
    Left e -> error $ "Invalid parameters:" ++ e
    Right r -> case r of
      Left e -> error $ "Error parsing input:" ++ show e
      Right v -> v
