{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.GCode

import Prelude hiding (readFile, putStrLn)

import Control.Applicative

import Pipes
import Pipes.Attoparsec as PA
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as B
--import Pipes.ByteString.MMap
import Pipes.Safe
import Pipes.Aeson.Unchecked (encode)
import qualified System.IO as IO

import Data.Aeson (decode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import Options.Applicative
import Options.Applicative.Builder

data Flags = Flags {
    input :: FilePath
  , output :: FilePath
  , json :: Bool
  , pretty :: Bool
  , analyze :: Bool
  } deriving (Show)

-- switch to arguments when following is fixed and tab-completion
-- prefers arguments before options
-- https://github.com/pcapriotti/optparse-applicative/issues/173
--  <$> argument str (metavar "FILE")

flags :: Parser Flags
flags = Flags
    <$> strOption ( long "input" <> short 'i' <> metavar "INPUT")
    <*> strOption ( long "output" <> short 'o' <> metavar "OUTPUT" <> value "")
    <*> switch ( long "json" <> short 'j' <> help "JSON output")
    <*> switch ( long "pretty" <> short 'p' <> help "Pretty output")
    <*> switch ( long "analyze" <> short 'a' <> help "Analyze GCode")

main =
    execParser opts >>= run
    where
      opts = info (helper <*> flags)
        ( fullDesc
       <> progDesc "Process GCode from INPUT"
       <> header "gcodehs - GCode processor" )

run :: Flags -> IO ()
run Flags{..} =
    if analyze
        then do
            r <- foldedpipe input analyzefold
            print r
        else case output of
              "" -> gcodepipe input $ fmt json pretty >-> B.stdout
              _  -> IO.withFile output IO.WriteMode $ \outhandle ->
                      gcodepipe input $ fmt json pretty >-> B.toHandle outhandle

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

analyzefold = P.fold step 0 id
  where step x a = x + 1 -- travel a

--analyzefold' = P.fold step 0 id
--  where step x a | isG a = x + 1
--        step x a | otherwise = x

fmt True _  = for Pipes.cat encode
fmt False True  = P.map ppGCodeLine >-> P.map (BS.pack . (++"\n"))
fmt False False = P.map ppGCodeLineCompact >-> P.map (BS.pack . (++"\n"))

-- doesn't work, probably due to bad FromJSON for maps
readjsonmain = do
  f <- BL.readFile "a.json"
  print (decode f :: Maybe GCode)

-- mmaped version, requires pipes-bytestring-mmap
--main' = do
--  file    <- fmap Prelude.head getArgs
--  runSafeT . runEffect $
--    (() <$ PA.parsed parseGCodeLine (unsafeMMapFile file) )
--    >-> P.map ppGCodeLine
--    >-> P.stdoutLn
