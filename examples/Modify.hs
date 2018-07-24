import Data.GCode

import Pipes
import Pipes.Attoparsec as PA
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as B
import Pipes.Safe
import qualified System.IO as IO
import qualified System.Environment as E

import GHC.Base

bufsize = 1024

-- complex modify

main :: IO ()
main = do
  file    <- fmap Prelude.head E.getArgs
  IO.withFile file IO.ReadMode $ \handle ->
    runSafeT . runEffect $
      (() <$ PA.parsed parseGCodeLine (B.hGetSome bufsize handle) )
      >-> P.filter (liftM2 (||) isRapid isMove)
--      >-> P.filter (isRapid)
      >-> P.map (modifyFeedrate (pure 666))
--      >-> P.map (replaceY 3.14)
--      >-> P.map (modifyAxes [X,Z] (*1000))
--      >-> P.map (modifyXY (\x y -> (x**2, y-100)))
      >-> P.map (modifyXY (rot (2.5*pi/2)))
--      >-> P.map ppGCodeLine
      >-> P.map ppGCodeLineCompact
      >-> P.stdoutLn
