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
      >-> P.filter isRapid
      >-> P.filter hasFeedrate
      >-> P.map (replaceFeedrate 666)
      >-> P.map (replaceY 3.14)
      >-> P.map (addReplaceZ 48)
      >-> P.map ppGCodeLine
-- or
--      >-> P.map ppGCodeLineCompact
      >-> P.stdoutLn
--
-- or to json
--      >-> for Pipes.cat encode
--      >-> B.stdout
--
-- non-streaming version, will eat a lot of ram
--main = do
--    file <- fmap Prelude.head getArgs
--    f <- BS.readFile file
--    case parseOnlyGCode f of
--        Left err -> print err
--        Right result -> BS.putStr $ BS.pack $ ppGCode result
