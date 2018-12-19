{-# LANGUAGE TemplateHaskell #-}
module Data.GCode.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Data.Char
import qualified GHC.Base

-- this walks constructors of a datatype
-- and creates isXYZ checks and CodeMod constructors
-- for example for constructor `Rapid` these two are generated
-- isRapid :: Code -> Bool
-- isRapid x = x `codeIsRS274` Rapid
--
-- rapid :: CodeMod
-- rapid = codeModFromName Rapid
genShortcuts names = do
  info <- reify names
  case info of
    TyConI (DataD _cxt _name _tyvarbndr _kind constructors _deriv)
      -> do
        a <- mapM genTests constructors
        b <- mapM genConstructors constructors
        return $ a ++ b

  where
    genTests (NormalC name _bangs) = do
      varName <- newName "x"
      let
        funName = mkName $ "is" ++ (nameBase name)

      return $ FunD funName
        [ Clause
           [VarP varName]
           (NormalB (InfixE (Just (VarE varName)) (VarE (mkName "codeIsRS274")) (Just (ConE name))))
           []
        ]

    genConstructors (NormalC name _bangs) = do
      let
        funName = mkName $ (\(x:rest) -> (Data.Char.toLower x : rest)) (nameBase name)
      return $ FunD funName
        [ Clause
          []
          (NormalB ( (VarE (mkName "codeModFromName")) `AppE` (ConE name)) )
          []
        ]
