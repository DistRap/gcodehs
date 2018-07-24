import Data.GCode

import Pipes
import Pipes.Attoparsec as PA
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as B
import Pipes.Safe
import qualified System.IO as IO
import qualified System.Environment as E

bufsize = 1024

main :: IO ()
main = do
  file    <- fmap Prelude.head E.getArgs
  IO.withFile file IO.ReadMode $ \handle ->
    runSafeT . runEffect $
      (() <$ PA.parsed parseGCodeLine (B.hGetSome bufsize handle) )
      >-> P.map ppGCodeLineCompact
      >-> P.stdoutLn
