{-# LANGUAGE OverloadedStrings #-}
module GCode.Parse (parseGCode, parseGCodeLine, parseOnlyGCode) where

import GCode.Types

import Prelude hiding (take, takeWhile, mapM)
import Control.Applicative
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8
import Debug.Trace
import qualified Data.Map.Strict as M

import Data.Either

lskip = skipWhile (inClass " \t")
between open close p = do{ open; x <- p; close; return x }

isEndOfLine_chr :: Char -> Bool
isEndOfLine_chr '\n' = True
isEndOfLine_chr '\r' = True
isEndOfLine_chr _ = False

parseLead = do
    a <- satisfy $ inClass "GMTPFS"
    return $ codecls a

parseAxisDes = do
    a <- satisfy $ inClass "XYZABCEL"
    return $ axis a

parseParamDes = do
    a <- satisfy $ inClass "SPF"
    return $ param a

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

parseCode = do
    lead <- parseLead
    gcode <- decimal
    subcode <- option 0 (Just <$> char '.' *> decimal)
    lskip
    (axes, params) <- parseAxesParams
    lskip
    comment <- option "" $ between lskip lskip parseComment'
    return $ Code lead gcode subcode axes params comment

parseComment' = do
    t <- many $ between (lskip *> char '(') (char ')' <* lskip) $ takeWhile1 (/=')')
    -- semiclone prefixed comments
    semisep <- option "" $ char ';' *> takeWhile (not . isEndOfLine_chr)
    rest <- takeWhile (not . isEndOfLine_chr)
    return $ B.concat $ t ++ [semisep, rest]

parseComment = do
    Comment <$> parseComment'

parseOther = do
    a <- takeWhile (not . isEndOfLine_chr)
    return $ Other a

parseCodeParts = do
           parseCode
      <|>  parseComment
      <|>  parseOther

parseGCodeLine = between lskip lskip parseCodeParts <* endOfLine

parseGCode :: Parser GCode
parseGCode = do
      many1 parseGCodeLine

parseOnlyGCode = parseOnly parseGCode
