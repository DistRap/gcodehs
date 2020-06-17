{-# LANGUAGE OverloadedStrings #-}
module Data.GCode.Pipes where

import Control.Monad
import Control.Monad.Trans.State.Strict

import Data.ByteString (ByteString)

import System.IO (Handle)

import Data.GCode.Types
import Data.GCode.Canon
import Data.GCode.Eval
import Data.GCode.Line
import Data.GCode.Parse
import Data.GCode.Pretty
import qualified Data.GCode.Canon.Convert

import Pipes
import Pipes.Attoparsec (ParsingError)
import Pipes.Safe (SafeT)

import qualified Data.ByteString.Char8
import qualified Data.Map.Strict
import qualified Pipes.Attoparsec
import qualified Pipes.ByteString
import qualified Pipes.Prelude
import qualified Pipes.Safe
import qualified System.IO

-- something fishy about this type
parseProducer :: Handle -> Producer Code (SafeT IO) (Either (ParsingError, Producer ByteString (SafeT IO) ()) ())
parseProducer = parseProducer' 1024

parseProducer' :: MonadIO m
               => Int
               -> Handle
               -> Producer Code m (Either (ParsingError, Producer ByteString m ()) ())
parseProducer' bufSize handle = Pipes.Attoparsec.parsed
  parseGCodeLine (Pipes.ByteString.hGetSome bufSize handle)

withFile :: FilePath -> (Handle -> (SafeT IO) r) -> IO r
withFile filepath job =
  System.IO.withFile filepath System.IO.ReadMode $ \handle ->
    Pipes.Safe.runSafeT $ job handle

pipeToList :: FilePath -> Proxy () Code () a (SafeT IO) () -> IO [a]
pipeToList filepath pipeTail = withFile filepath $ \h ->
  Pipes.Prelude.toListM
    $ (() <$ parseProducer h)
      >-> pipeTail

gcodeToCanonList :: FilePath -> IO [Canon]
gcodeToCanonList filepath = pipeToList filepath $ evalP >-> evalCanonP

gcodeToLines :: FilePath -> IO [Line]
gcodeToLines filepath = pipeToList filepath $ evalP >-> evalCanonLinesP

gcodePipe :: FilePath -> (Consumer Code (SafeT IO) ()) -> IO ()
gcodePipe filepath pipeTail =
  System.IO.withFile filepath System.IO.ReadMode $ \handle ->
    Pipes.Safe.runSafeT . runEffect $
      (() <$ parseProducer handle)
      >-> pipeTail

-- needs better name
runPipe :: FilePath
        -> Maybe FilePath
        -> (Pipe Code ByteString (SafeT IO) ())
        -> IO ()
runPipe input Nothing pipeMiddle = gcodePipe input (pipeMiddle >-> Pipes.ByteString.stdout)
runPipe input (Just output) pipeMiddle =
  System.IO.withFile output System.IO.WriteMode $ \outhandle ->
    gcodePipe input (pipeMiddle >-> Pipes.ByteString.toHandle outhandle)


foldedPipe :: FilePath
           -> (Producer Code (Pipes.Safe.SafeT IO) () -> Effect (Pipes.Safe.SafeT IO) r)
           -> IO r
foldedPipe filepath fold =
  System.IO.withFile filepath System.IO.ReadMode $ \handle ->
    Pipes.Safe.runSafeT . runEffect $
        fold (() <$ parseProducer handle)

-- evaluators

totalizeP :: Pipe Code Code (SafeT IO) ()
totalizeP = flip evalStateT Data.Map.Strict.empty $ forever $ do
  x <- lift await
  inEffect <- get
  let updatedCode = updateFromCurrentModals inEffect x
      updatedModals = updateModals inEffect updatedCode

  put updatedModals
  lift $ yield updatedCode

evalP :: Pipe Code Code (SafeT IO) ()
evalP = flip evalStateT newState $ forever $ do
  x <- lift await
  st <- get
  let (result, steppedState, _rest) = step st [x]
  -- XXX: add pretty printer for IPState
  --liftIO $ print steppedState
  put steppedState
  case result of
    Just r -> lift $ yield r
    Nothing -> return ()

evalCanonP :: Pipe Code Canon (SafeT IO) ()
evalCanonP = flip evalStateT initCanonState $ forever $ do
  x <- lift await
  st <- get

  forM_ (Data.GCode.Canon.Convert.toCanon x) $ \c -> do
    let steppedState = stepCanon st c
    put steppedState
    lift $ yield c

evalCanonLinesP :: Pipe Code Line (SafeT IO) ()
evalCanonLinesP = flip evalStateT initCanonState $ forever $ do
  x <- lift await
  st <- get

  forM_ (Data.GCode.Canon.Convert.toCanon x) $ \c -> do
    let steppedState = stepCanon st c
    put steppedState
    forM_ (toLines st steppedState c) $ lift . yield

-- mmaped experiment, requires pipes-bytestring-mmap
--import qualified Pipes.ByteString.MMap
--main' = do
--  file    <- fmap Prelude.head getArgs
--  Pipes.Safe.runSafeT . Pipes.Safe.runEffect $
--    (() <$ Pipes.Attoparsec.parsed parseGCodeLine (Pipes.ByteString.MMap.unsafeMMapFile file) )
--    >-> Pipes.Prelude.map ppGCodeLine
--    >-> Pipes.Prelude.stdoutLn

-- pretty print
prettySinkWith :: (a -> ByteString) -> Pipe a ByteString (SafeT IO) ()
prettySinkWith fn =
      Pipes.Prelude.map fn

prettySink :: Pipe Code ByteString (SafeT IO) ()
prettySink =
      Pipes.Prelude.map ppGCodeLine
  >-> Pipes.Prelude.map (Data.ByteString.Char8.pack . (++"\n"))

compactSink :: Pipe Code ByteString (SafeT IO) ()
compactSink =
      Pipes.Prelude.map ppGCodeLineCompact
  >-> Pipes.Prelude.map (Data.ByteString.Char8.pack . (++"\n"))

-- Helpers

addNewLine :: ByteString -> ByteString
addNewLine to = Data.ByteString.Char8.append to "\n"

wrapPrinter :: (a -> String) -> a -> ByteString
wrapPrinter p = addNewLine . Data.ByteString.Char8.pack . p
