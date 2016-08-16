import Data.GCode

import qualified System.IO as IO
import qualified System.Environment as E

import qualified Data.ByteString.Char8 as BS

-- non-streaming version, will eat a lot of ram
-- if used on bigger files, only for demo purposes
main = do
    file <- fmap Prelude.head E.getArgs
    f <- BS.readFile file
    case parseOnlyGCode f of
        Left err -> print err
        Right result -> BS.putStr $ BS.pack $ ppGCode result
