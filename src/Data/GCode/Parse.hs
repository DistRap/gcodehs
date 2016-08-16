{-| GCode parsing functions
-}

{-# LANGUAGE OverloadedStrings #-}
module Data.GCode.Parse (parseGCode, parseGCodeLine, parseOnlyGCode) where

import Data.GCode.Types

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


lskip = skipWhile (inClass " \t")
between open close p = do{ open; x <- p; close; return x }

isEndOfLineChr :: Char -> Bool
isEndOfLineChr '\n' = True
isEndOfLineChr '\r' = True
isEndOfLineChr _ = False

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
