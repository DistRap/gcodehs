import Data.GCode

import Pipes
import Pipes.Attoparsec as PA
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as B
import Pipes.Safe
import Pipes.Aeson.Unchecked (encode)
import qualified System.IO as IO
import qualified System.Environment as E

import GHC.Base

bufsize = 1024

-- rotate by angle
angle = 90

main :: IO ()
main = do
  file    <- fmap Prelude.head E.getArgs
  IO.withFile file IO.ReadMode $ \handle ->
    runSafeT . runEffect $
      (() <$ PA.parsed parseGCodeLine (B.hGetSome bufsize handle) )
      >-> P.map (modifyXY (rot (-angle*pi/2)))
      >-> P.map ppGCodeLineCompact
      >-> P.stdoutLn
