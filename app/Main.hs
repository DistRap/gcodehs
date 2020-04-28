{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.GCode

import Prelude hiding (readFile, putStrLn)

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict

import Pipes
import Pipes.Attoparsec as PA
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as B
--import Pipes.ByteString.MMap
import Pipes.Safe
import qualified System.IO as IO

import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Double.Conversion.ByteString

import Options.Applicative
import Options.Applicative.Builder

data Command =
    Cat
  | Pretty
  | Analyze
  | Limits
  | ToOrigin
  | Totalize
  | TranslateXY Double Double
  | TranslateZ Double
  | Rotate Double
  | ScaleFeedrate Double
  | ScaleXY Double Double
  -- TODO
  | TravelDistance
  | ExtrusionDistance
  | Generate
  | Eval
  | Canon
  deriving (Eq, Show, Ord)

data Options = Options {
    cmd     :: Command
  , input   :: FilePath
  , output  :: FilePath
  } deriving (Show)

cmdParser :: Parser Command
cmdParser = subparser
  (  command "cat" (info (pure Cat) (progDesc "Parse and print GCode"))
  <> command "pretty" (info (pure Pretty) (progDesc "Parse and pretty-print GCode"))
  <> command "limits" (info (pure Limits) (progDesc "Compute axis movements limits"))
  <> command "move-to-origin" (info (pure ToOrigin) (progDesc "Move GCode so it starts at X0 Y0"))
  <> command "totalize" (info (pure Totalize) (progDesc "Walk GCode adding missing axes coordinates according to previous moves"))
  <> command "translate-xy" (info (TranslateXY <$> argument auto (metavar "X") <*> argument auto (metavar "Y")) (progDesc "Translate GCode by X Y offsets"))
  <> command "translate-z" (info (TranslateZ <$> argument auto (metavar "Z")) (progDesc "Translate GCode by Z offset"))
  <> command "rotate" (info (Rotate <$> argument auto (metavar "DEG")) (progDesc "Rotate GCode by angle in degrees"))
  <> command "scale-feedrate" (info (ScaleFeedrate <$> argument auto (metavar "MULTIPLIER")) (progDesc "Scale feedrates by multiplier"))
  <> command "scale-xy" (info (ScaleXY <$> argument auto (metavar "X") <*> argument auto (metavar "Y")) (progDesc "Scale X/Y by multiplier"))
  <> command "eval" (info (pure Eval) (progDesc "Evaluate GCode"))
  <> command "canon" (info (pure Canon) (progDesc "Convert to canonical representation"))
  )

flags :: Parser Options
flags = Options
    <$> cmdParser
    <*> argument str (metavar "FILE")
    <*> strOption ( long "output" <> short 'o' <> metavar "OUTPUT" <> value "")

main =
    execParser opts >>= run
    where
      opts = info (helper <*> flags)
        ( fullDesc
       <> progDesc "Process GCode from FILE"
       <> header "gcodehs - GCode processor" )

pretty = True
analyze = False

run :: Options -> IO ()
run Options{..} =
    let
      runPipe = case output of
                  "" -> \pipemid -> gcodepipe input (pipemid >-> B.stdout)
                  _  -> \pipemid -> IO.withFile output IO.WriteMode $ \outhandle ->
                          gcodepipe input (pipemid >-> B.toHandle outhandle)

      foldPipe = foldedpipe input
      ppLimit (ax, (low, high)) = BS.intercalate "\t" [BS.pack $ show ax, toFixed 6 low, toFixed 6 high]

      ppCompact = P.map ppGCodeLineCompact >-> P.map (BS.pack . (++"\n"))
    in
      case cmd of
        Cat                       -> runPipe $ ppCompact

        Pretty                    -> runPipe $ P.map ppGCodeLine >-> P.map (BS.pack . (++"\n"))

        Limits                    -> do

                                        limits <- foldPipe $ P.fold (\s x -> updateLimitsCode s x) M.empty id
                                        mapM_ (BS.putStrLn . ppLimit) $ M.toList limits

        ToOrigin                  -> do

                                        limits <- foldPipe $ P.fold (\s x -> updateLimitsCode s x) M.empty id

                                        case (M.lookup X limits, M.lookup Y limits) of
                                         (Just (xmin, xmax), Just (ymin, ymax)) -> do
                                           let mov = \x y -> (x - xmin, y - ymin)
                                           runPipe $ P.map (modifyXY (mov)) >-> ppCompact

                                         _ -> do BS.putStrLn "X or Y limit(s) not found"

        Totalize                  -> runPipe $ tot >-> ppCompact
        Eval                      -> runPipe $ evalP >-> ppCompact
        TranslateXY xtrans ytrans -> runPipe $ P.map (modifyXY (\x y -> (x + xtrans, y + ytrans))) >-> ppCompact
        TranslateZ ztrans         -> runPipe $ P.map (modifyAxis Z (+ztrans)) >-> ppCompact
        Rotate angle              -> runPipe $ P.map (modifyXY (rot (angle*pi/180))) >-> ppCompact
        ScaleFeedrate factor      -> runPipe $ P.map (modifyFeedrate (*factor)) >-> ppCompact
        ScaleXY xsc ysc           -> runPipe $ P.map (modifyXY (\x y -> (x*xsc, y*ysc))) >-> ppCompact

        _ -> fail "no such cmd"
    {-
    if analyze
        then do
            r <- foldedpipe input analyzefold
            print r
        else case output of
              "" -> gcodepipe input $ fmt pretty >-> B.stdout
              _  -> IO.withFile output IO.WriteMode $ \outhandle ->
                      gcodepipe input $ fmt pretty >-> B.toHandle outhandle
    -}

tot = flip evalStateT M.empty $ forever $ do
  x <- lift await
  inEffect <- get
  let updatedCode = updateFromCurrentModals inEffect x
      updatedModals = updateModals inEffect updatedCode

  put updatedModals
  lift $ yield updatedCode

evalP = flip evalStateT newState $ forever $ do
  x <- lift await
  st <- get
  let (result, steppedState, _rest) = step st [x]
  liftIO $ print steppedState
  put steppedState
  case result of
    Just r -> lift $ yield r
    Nothing -> return ()

bufsize = 1024

parseProducer handle = PA.parsed parseGCodeLine (B.hGetSome bufsize handle)

gcodepipe filepath tail =
  IO.withFile filepath IO.ReadMode $ \handle ->
    runSafeT . runEffect $
      (() <$  parseProducer handle)
      >-> tail

foldedpipe filepath fold =
  IO.withFile filepath IO.ReadMode $ \handle ->
      runSafeT . runEffect $
        fold (() <$  parseProducer handle)


{-
pp = BS.pack . (++"\n") . ppGCodeLine
ppExp code = (BS.pack $ explain code ++ "\n") `BS.append` (pp code)

fmt _ = P.map ppExp
-}

-- mmaped version, requires pipes-bytestring-mmap
--main' = do
--  file    <- fmap Prelude.head getArgs
--  runSafeT . runEffect $
--    (() <$ PA.parsed parseGCodeLine (unsafeMMapFile file) )
--    >-> P.map ppGCodeLine
--    >-> P.stdoutLn
