{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.GCode.Pretty(ppGCode, ppGCodeLine, ppGCodeCompact, ppGCodeLineCompact) where

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Formatting (sformat)
import Formatting.ShortFormatters

import Text.PrettyPrint.ANSI.Leijen
import Data.GCode.Types

ppGCode res = displayS (renderPretty 0.4 80 (ppGCode' res)) ""
ppGCodeLine res = displayS (renderPretty 0.4 80 (ppCode res)) ""
ppGCodeCompact res = displayS (renderCompact (ppGCode' res)) ""
ppGCodeLineCompact res = displayS (renderCompact (ppCode res)) ""

ppList pp x = hsep $ map pp x

ppGCode' = vsep . map ppCode

ppClass G = yellow $ text "G"
ppClass M = red $ text "M"
ppClass T = magenta $ text "T"
ppClass StP = red $ text "P"
ppClass StF = red $ text "F"
ppClass StS = red $ text "S"

--codecolor Code{..} = cc cls code
cc G 0 = dullyellow
cc G 1 = yellow
cc _ _ = red

ppAxis (des, val) =
       bold (axisColor des $ text $ show des)
    <> cyan (text $ T.unpack $ sformat sf val)

axisColor X = red
axisColor Y = green
axisColor Z = yellow
axisColor A = red
axisColor B = green
axisColor C = blue
axisColor E = magenta

ppAxes [] = empty
ppAxes x = space <> ppList ppAxis x

ppParam (des, val) =
       bold (blue $ text $ show des)
    <> white (text $ T.unpack $ sformat sf val)

ppParams [] = empty
ppParams x = space <> ppList ppParam x

ppComment "" = empty
ppComment  c = space <> ppComment' c
ppComment' "" = empty
ppComment' c = dullwhite $ parens $ text $ unpack c

ppCode Code{..} =
       cc cls code ( bold $ ppClass cls)
    <> cc cls code ( text $ show code)
    <> ppAxes (M.toList axes)
    <> ppParams (M.toList params)
    <> ppComment comment
ppCode (Comment x) = ppComment' x
ppCode (Other x) = dullred $ text $ unpack x
