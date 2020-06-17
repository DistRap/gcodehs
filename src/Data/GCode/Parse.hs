{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-| GCode parsing functions
-}

module Data.GCode.Parse (parseGCode, parseGCodeLine, parseOnlyGCode) where

import Data.GCode.Types

import Control.Applicative

import Prelude hiding (take, takeWhile, mapM)
import Data.Attoparsec.ByteString.Char8

import Data.ByteString (ByteString)

import qualified Data.ByteString
import qualified Data.Char
import qualified Data.Either
import qualified Data.Map
import qualified Data.Maybe

-- |Parse single line of G-code into 'Code'
parseGCodeLine :: Parser Code
parseGCodeLine = between lskip lskip parseCodeParts <* endOfLine

-- |Parse lines of G-code into 'GCode'
parseGCode :: Parser GCode
parseGCode = many1 parseGCodeLine

-- |Parse lines of G-code returning either parsing error or 'GCode'
parseOnlyGCode :: ByteString -> Either String GCode
parseOnlyGCode = parseOnly parseGCode

lskip :: Parser ()
lskip = skipWhile (\x -> x == ' ' || x == '\t')

between :: Monad m => m a1 -> m a2 -> m b -> m b
between open close p = do { _ <- open; x <- p; _ <- close; return x }

isEndOfLineChr :: Char -> Bool
isEndOfLineChr '\n' = True
isEndOfLineChr '\r' = True
isEndOfLineChr _ = False

parseLead :: Parser Class
parseLead = do
    a <- satisfy $ inClass $ (asChars allClasses) ++ (map Data.Char.toLower $ asChars allClasses)
    return $ Data.Maybe.fromJust $ toCodeClass a
{-# INLINE parseLead #-}

parseAxisDes :: Parser AxisDesignator
parseAxisDes = do
    a <- satisfy $ inClass $ asChars allAxisDesignators
    return $ Data.Maybe.fromJust $ toAxis a
{-# INLINE parseAxisDes #-}

parseParamDes :: Parser ParamDesignator
parseParamDes = do
    a <- satisfy $ inClass $ asChars allParamDesignators
    return $ Data.Maybe.fromJust $ toParam a
{-# INLINE parseParamDes #-}

parseParamOrAxis :: Parser (Either (AxisDesignator, Double) (ParamDesignator, Double))
parseParamOrAxis = do
    lskip
    ax <- option Nothing (Just <$> parseAxisDes)
    case ax of
      Just val -> do
          lskip
          f <- double
          return $ Left (val, f)
      Nothing -> do
          paramDes <- parseParamDes
          lskip
          f <- double
          return $ Right (paramDes, f)

parseAxesParams :: Parser (Axes, Params)
parseAxesParams = do
    a <- many parseParamOrAxis
    return (Data.Map.fromList $ Data.Either.lefts a, Data.Map.fromList $ Data.Either.rights a)
{-# INLINE parseAxesParams #-}

parseCode :: Parser Code
parseCode = do
    codeCls <- optional parseLead
    codeNum <- optional decimal
    codeSub <- optional (char '.' *> decimal)
    lskip
    (codeAxes, codeParams) <- parseAxesParams
    lskip
    codeComment <- option "" $ between lskip lskip parseComment'
    let c = Code{..}
    if c == emptyCode
      then return $ Empty
      else return c

parseComment' :: Parser ByteString
parseComment' = do
    t <- many $ between (lskip *> char '(') (char ')' <* lskip) $ takeWhile1 (/=')')
    -- semiclone prefixed comments
    semisep <- option "" $ char ';' *> takeWhile (not . isEndOfLineChr)
    rest <- takeWhile (not . isEndOfLineChr)
    return $ Data.ByteString.concat $ t ++ [semisep, rest]

parseComment :: Parser Code
parseComment = Comment <$> parseComment'

parseOther :: Parser Code
parseOther = do
    a <- takeWhile (not . isEndOfLineChr)
    return $ Other a

parseCodeParts :: Parser Code
parseCodeParts =
           parseCode
      <|>  parseOther
      <|>  parseComment
