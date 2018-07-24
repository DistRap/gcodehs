{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.GCode

import Pipes
import Pipes.Attoparsec as PA
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as B
import Pipes.Safe
import qualified System.IO as IO
import qualified System.Environment as E

import Formatting (sformat)
import Formatting.ShortFormatters

import Data.Map as M
import Data.Text as T
import Data.Text.IO as TIO

bufsize = 1024

ppLimit (ax, (low, high)) = T.intercalate " " [T.pack $ show ax, (sformat sf low), (sformat sf high)]
foldLimits = P.fold (\s x -> updateLimitsCode s x) M.empty id



main :: IO ()
main = do
  file    <- fmap Prelude.head E.getArgs
  lim <- IO.withFile file IO.ReadMode $ \handle ->
    runSafeT . runEffect $
      foldLimits (() <$ PA.parsed parseGCodeLine (B.hGetSome bufsize handle) )

  case (M.lookup X lim, M.lookup Y lim) of
    (Just (xmin, xmax), Just (ymin, ymax)) -> do
      let mov = \x y -> (x - xmin, y - ymin)

      IO.withFile file IO.ReadMode $ \handle ->
        runSafeT . runEffect $
          (() <$ PA.parsed parseGCodeLine (B.hGetSome bufsize handle) )
          >-> P.map (modifyXY (mov))
          >-> P.map ppGCodeLineCompact
          >-> P.stdoutLn
    _ -> do TIO.putStrLn "X or Y limit(s) not found"

