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

-- compute gcode limits

ppLimit (ax, (low, high)) = T.intercalate " " [T.pack $ show ax, (sformat sf low), (sformat sf high)]
foldLimits = P.fold (\s x -> updateLimitsCode s x) M.empty id


main :: IO ()
main = do
  file    <- fmap Prelude.head E.getArgs
  a <- IO.withFile file IO.ReadMode $ \handle ->
    runSafeT . runEffect $
      foldLimits (() <$ PA.parsed parseGCodeLine (B.hGetSome bufsize handle) )

  mapM_ (TIO.putStrLn . ppLimit) $ M.toList a

