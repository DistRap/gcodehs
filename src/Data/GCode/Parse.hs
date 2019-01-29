{-| GCode parsing functions
-}

{-# LANGUAGE OverloadedStrings #-}
module Data.GCode.Parse (parseGCode, parseGCodeLine, parseOnlyGCode) where

import Data.GCode.Types

import Data.Char (toLower)
import Data.Maybe (fromJust)
import Prelude hiding (take, takeWhile, mapM)
import Control.Applicative
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8
import qualified Data.Map.Strict as M

import Data.Either (lefts, rights)

-- |Parse single line of G-code into 'Code'
parseGCodeLine :: Parser Code
parseGCodeLine = between lskip lskip parseCodeParts <* endOfLine

-- |Parse lines of G-code into 'GCode'
parseGCode :: Parser GCode
parseGCode = many1 parseGCodeLine

-- |Parse lines of G-code returning either parsing error or 'GCode'
parseOnlyGCode :: B.ByteString -> Either String GCode
parseOnlyGCode = parseOnly parseGCode


lskip = skipWhile (\x -> x == ' ' || x == '\t')
between open close p = do{ open; x <- p; close; return x }

isEndOfLineChr :: Char -> Bool
isEndOfLineChr '\n' = True
isEndOfLineChr '\r' = True
isEndOfLineChr _ = False

parseLead = do
    a <- satisfy $ inClass $ (asChars allClasses) ++ (map toLower $ asChars allClasses)
    return $ fromJust $ toCodeClass a
{-# INLINE parseLead #-}

parseAxisDes = do
    a <- satisfy $ inClass $ asChars allAxisDesignators
    return $ fromJust $ toAxis a
{-# INLINE parseAxisDes #-}

parseParamDes = do
    a <- satisfy $ inClass $ asChars allParamDesignators
    return $ fromJust $ toParam a
{-# INLINE parseParamDes #-}

parseParamOrAxis = do
    lskip
    ax <- option Nothing (Just <$> parseAxisDes)
    case ax of
      Just val -> do
          lskip
          f <- double
          return $ Left (val, f)
      Nothing -> do
          param <- parseParamDes
          lskip
          f <- double
          return $ Right (param, f)

parseAxesParams :: Parser (Axes, Params)
parseAxesParams = do
    a <- many parseParamOrAxis
    return (M.fromList $ lefts a, M.fromList $ rights a)
{-# INLINE parseAxesParams #-}


parseCode = do
    lead <- optional parseLead
    gcode <- optional decimal
    subcode <- optional (char '.' *> decimal)
    lskip
    (axes, params) <- parseAxesParams
    lskip
    comment <- option "" $ between lskip lskip parseComment'
    let c = Code lead gcode subcode axes params comment
    if c == emptyCode
      then return $ Empty
      else return c

parseComment' = do
    t <- many $ between (lskip *> char '(') (char ')' <* lskip) $ takeWhile1 (/=')')
    -- semiclone prefixed comments
    semisep <- option "" $ char ';' *> takeWhile (not . isEndOfLineChr)
    rest <- takeWhile (not . isEndOfLineChr)
    return $ B.concat $ t ++ [semisep, rest]

parseComment = Comment <$> parseComment'

parseOther = do
    a <- takeWhile (not . isEndOfLineChr)
    return $ Other a

parseCodeParts =
           parseCode
      <|>  parseComment
      <|>  parseOther
