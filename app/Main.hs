{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import Pipes (Pipe, (>->))
import Pipes.Safe (SafeT)
import Data.ByteString (ByteString)

import Control.Applicative
import Options.Applicative

import Data.GCode
import Data.GCode.Pipes.Transform
import Data.GCode.Line (prettyLine)

data Command =
    Cat
  | Pretty
  | Analyze
  | Totalize
  | TranslateXY Double Double
  | TranslateZ Double
  | Rotate Double
  | ScaleFeedrate Double
  | ScaleXY Double Double
  | Eval
  | Canon
  | Lines
  -- TODO
  | TravelDistance
  | ExtrusionDistance
  | Generate
  -- disabled for now as they require interepreter not just looking at coords
  -- (due to relative moves)
  | Limits
  | ToOrigin
  deriving (Eq, Show, Ord)

data Options = Options {
    cmd     :: Command
  , input   :: FilePath
  , output  :: Maybe FilePath
  } deriving (Show)

cmdParser :: Parser Command
cmdParser = subparser
  (  command "cat" (info (pure Cat) (progDesc "Parse and print GCode"))
  <> command "pretty" (info (pure Pretty) (progDesc "Parse and pretty-print GCode"))
--  <> command "limits" (info (pure Limits) (progDesc "Compute axis movements limits"))
--  <> command "move-to-origin" (info (pure ToOrigin) (progDesc "Move GCode so it starts at X0 Y0"))
  <> command "totalize" (info (pure Totalize) (progDesc "Walk GCode adding missing axes coordinates according to previous moves"))
  <> command "translate-xy" (info (TranslateXY <$> argument auto (metavar "X") <*> argument auto (metavar "Y")) (progDesc "Translate GCode by X Y offsets"))
  <> command "translate-z" (info (TranslateZ <$> argument auto (metavar "Z")) (progDesc "Translate GCode by Z offset"))
  <> command "rotate" (info (Rotate <$> argument auto (metavar "DEG")) (progDesc "Rotate GCode by angle in degrees"))
  <> command "scale-feedrate" (info (ScaleFeedrate <$> argument auto (metavar "MULTIPLIER")) (progDesc "Scale feedrates by multiplier"))
  <> command "scale-xy" (info (ScaleXY <$> argument auto (metavar "X") <*> argument auto (metavar "Y")) (progDesc "Scale X/Y by multiplier"))
  <> command "eval" (info (pure Eval) (progDesc "Evaluate GCode"))
  <> command "canon" (info (pure Canon) (progDesc "Convert to canonical representation"))
  <> command "lines" (info (pure Lines) (progDesc "Convert to lines"))
  )

flags :: Parser Options
flags = Options
    <$> cmdParser
    <*> argument str (metavar "INPUT-FILE")
    <*> (optional
      $ strOption (
             long "output"
          <> short 'o'
          <> metavar "OUTPUT-FILE"))

main :: IO ()
main =
    execParser opts >>= \Options{..} -> runPipe input output (toSink cmd)
    where
      opts = info (helper <*> flags)
        ( fullDesc
       <> progDesc "Process GCode from FILE"
       <> header "gcodehs - GCode processor" )

toSink :: Command -> Pipe Code ByteString (SafeT IO) ()
toSink Cat      = compactSink
toSink Pretty   = prettySink
toSink Totalize = totalizeP >-> compactSink
toSink Eval     = evalP >-> prettySink
toSink Canon    = evalP >-> evalCanonP >-> prettySinkWith (wrapPrinter show)
toSink Lines    = evalP >-> evalCanonLinesP >-> (prettySinkWith $ wrapPrinter $ prettyLine defaultStyle)

toSink (TranslateXY xt yt) = translateXY xt yt >-> compactSink
toSink (TranslateZ  zt)    = translateZ zt     >-> compactSink
toSink (ScaleXY xs ys)     = scaleXY xs ys     >-> compactSink
toSink (ScaleFeedrate s)   = scaleFeedrate s   >-> compactSink
toSink (Rotate a)          = rotate a          >-> compactSink

toSink _ = error "Currently not supported, sorry :("
