{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Data.GCode.Pipes (
    runPipe
  , gcodePipe
  , pipeToList

  , evalP
  , evalCanonP
  , evalCanonStateP
  , evalCanonLinesP
  , totalizeP

  , gcodeToLines
  , gcodeToCanonList

  , compactSink
  , prettySink
  , prettySinkWith
  , wrapPrinter

  , gcodePipe'
  , pipeToList'
  , evalCanonLinesP'
  , evalCanonStateP'
  , trackAllLimits
  , trackWorkLimits

  ) where

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
import Pipes.Core
import Pipes.Attoparsec (ParsingError)
import Pipes.Safe (SafeT)

import qualified Data.ByteString.Char8
import qualified Data.Map.Strict
import qualified Pipes.Attoparsec
import qualified Pipes.ByteString
import qualified Pipes.Prelude
import qualified Pipes.Safe
import qualified System.IO

-- | Parse GCodes from @Handle@ producing @Code@ stream
parseProducer
  :: Handle
  -> Producer Code (SafeT IO) (Either (ParsingError, Producer ByteString (SafeT IO) ()) ())
parseProducer = parseProducer' 1024

-- | Generalized @parseProducer@ with buffer size parameter
parseProducer'
  :: MonadIO m
   => Int
   -> Handle
   -> Producer Code m (Either (ParsingError, Producer ByteString m ()) ())
parseProducer' bufSize handle = Pipes.Attoparsec.parsed
  parseGCodeLine (Pipes.ByteString.hGetSome bufSize handle)

-- | Run job with file handle in @SafeT IO@
withFile :: FilePath -> (Handle -> (SafeT IO) r) -> IO r
withFile filepath job =
  System.IO.withFile filepath System.IO.ReadMode $ \handle ->
    Pipes.Safe.runSafeT $ job handle

-- | Run pipe to completion and collect results as list
pipeToList :: FilePath -> Proxy () Code () a (SafeT IO) () -> IO [a]
pipeToList filepath pipeTail = withFile filepath $ \h ->
  Pipes.Prelude.toListM
    $ (() <$ parseProducer h)
      >-> pipeTail

-- | Evaluate GCode file to list of @Canon@s
gcodeToCanonList :: FilePath -> IO [Canon]
gcodeToCanonList filepath = pipeToList filepath $ evalP >-> evalCanonP

-- | Evaluate GCode file to list of @Line@s
gcodeToLines :: FilePath -> IO [Line]
gcodeToLines filepath = pipeToList filepath $ evalP >-> evalCanonLinesP

-- | Run @Consumer Code@ with input file
gcodePipe :: FilePath -> Consumer Code (SafeT IO) () -> IO ()
gcodePipe filepath pipeTail =
  withFile filepath $ \handle ->
    runEffect $
      (() <$ parseProducer handle)
      >-> pipeTail

-- | Run @Pipe Code ByteString (SafeT IO)@ with input file, optionally
-- writing contents to output file.
runPipe :: FilePath       -- ^ Input file
        -> Maybe FilePath -- ^ Nothing mean stdout, Just file output
        -> Pipe Code ByteString (SafeT IO) ()
        -> IO ()
runPipe input Nothing pipeMiddle = gcodePipe input (pipeMiddle >-> Pipes.ByteString.stdout)
runPipe input (Just output) pipeMiddle =
  System.IO.withFile output System.IO.WriteMode $ \outhandle ->
    gcodePipe input (pipeMiddle >-> Pipes.ByteString.toHandle outhandle)

-- evaluators

-- | Run stateful @Code@ evaluator, applying @totalize@
totalizeP :: Pipe Code Code (SafeT IO) ()
totalizeP = flip evalStateT Data.Map.Strict.empty $ forever $ do
  x <- lift await
  inEffect <- get
  let updatedCode = updateFromCurrentModals inEffect x
      updatedModals = updateModals inEffect updatedCode

  put updatedModals
  lift $ yield updatedCode

-- | Run stateful @Code@ evaluator.
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

-- | Stateful pipe evaluating `Code` to `Canon`
evalCanonP :: Pipe Code Canon (SafeT IO) ()
evalCanonP = flip evalStateT initCanonState $ forever $ do
  x <- lift await
  st <- get

  forM_ (Data.GCode.Canon.Convert.toCanon x) $ \c -> do
    let steppedState = stepCanon st c
    put steppedState
    lift $ yield c

-- | Stateful pipe evaluating `Code` to `Canon` `CanonState` tuples
-- Similar to @evalCanonP@ but also forwards @CanonState@ downstream.
evalCanonStateP :: Pipe Code (Canon, CanonState) (SafeT IO) ()
evalCanonStateP = flip evalStateT initCanonState $ forever $ do
  x <- lift await
  st <- get

  forM_ (Data.GCode.Canon.Convert.toCanon x) $ \c -> do
    let steppedState = stepCanon st c
    put steppedState
    lift $ yield (c, steppedState)

-- | Stateful pipe evaluating `Code` to `Line`
evalCanonLinesP :: Pipe Code Line (SafeT IO) ()
evalCanonLinesP = flip evalStateT initCanonState $ forever $ do
  x <- lift await
  st <- get

  forM_ (Data.GCode.Canon.Convert.toCanon x) $ \c -> do
    let steppedState = stepCanon st c
    put steppedState
    forM_ (toLines st steppedState c) $ lift . yield

-- * Pipes with termination including result values

type Downstreamed a =
  (Either
    (Either
      (ParsingError , Producer ByteString (SafeT IO) ())
      ()
    )
    a
  )

-- | Similar to @gcodePipe@ but uses @Downstreamed@
-- to indicate termination to downstream pipe with @Left@
--
-- Usage:
-- > gcodePipe' "./sample.gcode"
-- >   $ (fmap Left evalCanonStateP')
-- >     >-> (fmap Right trackAllLimits)
-- >     >-> (fmap Left (prettySinkWith (wrapPrinter Prelude.show)
-- >          >-> Pipes.ByteString.stdout))
gcodePipe'
 :: FilePath
 -> Proxy () (Downstreamed Code) () X (Pipes.Safe.SafeT IO) r
  -> IO (Either b r)
gcodePipe' filepath pipeTail =
  System.IO.withFile filepath System.IO.ReadMode $ \handle ->
    Pipes.Safe.runSafeT . runEffect $
      returnDownstream (parseProducer handle)
      >-> fmap Right pipeTail

-- | Similar to @pipeToList@ but uses @Downstreamed@
-- to indicate termination to downstream pipe with @Left@
--
-- Usage:
-- > pipeToList' "./sample.gcode"
-- >   $ (fmap Left evalCanonStateP' )
-- >     >-> (fmap Right trackWorkLimits)
pipeToList'
 :: FilePath
 -> Proxy () (Downstreamed Code) () a (Pipes.Safe.SafeT IO) r
 -> IO ([a], Either b r)
pipeToList' filepath pipeTail = withFile filepath $ \h ->
  Pipes.Prelude.toListM'
    $ returnDownstream (parseProducer h)
      >-> fmap Right pipeTail

-- | Turn `Proxy` into another `Proxy` capturing its return value and sending it downstream
-- in form of `Either`
returnDownstream :: Monad m => Proxy a' a b' b m r -> Proxy a' a b' (Either r b) m r'
returnDownstream = (forever . respond . Left) <=< (respond . Right <\\)

-- | Stateful pipe evaluating `Code` to `Canon` `CanonState` tuples.
-- Variant of @evalCanonState@ using @Downstreamed@, where Left
-- indicates time to stop evaluation.
evalCanonStateP' :: Pipe
  (Downstreamed Code) (Either () (Canon, CanonState)) (SafeT IO) ()
evalCanonStateP' = flip evalStateT initCanonState $ go
  where
  go = do
    x' <- lift await
    case x' of
      Left _ -> lift $ yield $ Left ()
      Right x -> do
        st <- get
        forM_ (Data.GCode.Canon.Convert.toCanon x) $ \c -> do
          let steppedState = stepCanon st c
          put steppedState
          lift $ yield $ Right (c, steppedState)
        go

-- | Wrapper for stateful evaluators where receiving
-- @Left _@ means query local state and use it as return value.
untilLeft
  :: Functor m
  => (t -> StateT b (Proxy () (Either a1 t) y' y m) a2)
  -> StateT b (Proxy () (Either a1 t) y' y m) b
untilLeft p = do
  x' <- lift await
  case x' of
    Left _ -> get
    Right x -> p x >> untilLeft p

-- | Track limits of working area, including travel moves
trackAllLimits:: (Monad m) => Pipe (Either () (Canon, CanonState)) (Canon, CanonState) m Limits
trackAllLimits =
  flip evalStateT mempty
    $ untilLeft
      $ \(c,s) -> do
            modify (`updateLimits` canonPosition s)
            lift $ yield (c, s)

-- | Track limits of working area, excluding travel moves
trackWorkLimits :: (Monad m) => Pipe (Either () (Canon, CanonState)) (Canon, CanonState) m Limits
trackWorkLimits =
  flip evalStateT mempty
    $ untilLeft
      $ \(c,s) -> do
            -- TODO: shouldn't ignore arcs
            -- TODO: maybe flip the logic to ignore @StraightTraverse@
            case c of
              StraightFeed _ -> modify (`updateLimits` canonPosition s)
              _              -> return ()

            lift $ yield (c, s)

-- | Stateful pipe evaluating `Canon` to `Line`
evalCanonLinesP' :: Pipe Canon Line (SafeT IO) ()
evalCanonLinesP' = flip evalStateT initCanonState $ forever $ do
  x <- lift await
  st <- get

  let steppedState = stepCanon st x
  put steppedState
  forM_ (toLines st steppedState x) $ lift . yield

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
prettySinkWith = Pipes.Prelude.map

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
