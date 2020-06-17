{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.GCode.Monad where

import Data.GCode.TH
import Data.GCode.Types
import Data.GCode.RS274.Types
import Data.GCode.RS274 (codeFromName)

import Control.Monad.Trans.Writer.Lazy
import Data.Semigroup hiding (option)

 -- this gives us someCode and someCode' shortcuts generated from RS274/Types.hs
-- so we can write
-- > myP = prog $ do
-- >          rapid (xy 5 10)
-- >          move (x 0)
$(genWriterEndos ''RS274Name)

data Program = Program { programCode :: GCode }
  deriving (Eq, Show)

type ProgramWriter a = Writer (Endo Program) a

gen :: Code -> ProgramWriter ()
gen c = tell $ Endo (\x -> x { programCode = c:(programCode x) } )

prog :: ProgramWriter a -> Program
prog builder = appEndo (execWriter (builder >> programEnd')) (Program mempty)

generateName = gen . codeFromName
generateNameArgs name endoF = gen $ codeFromName name & endoF
