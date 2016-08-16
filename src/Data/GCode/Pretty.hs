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

ppGCode res = displayS (renderPretty 0.4 80 (pp_gcode res)) ""
ppGCodeLine res = displayS (renderPretty 0.4 80 (pp_code res)) ""
ppGCodeCompact res = displayS (renderCompact (pp_gcode res)) ""
ppGCodeLineCompact res = displayS (renderCompact (pp_code res)) ""

pp_list pp x = hsep $ map pp x

pp_gcode = vsep . map pp_code

pp_class G = yellow $ text "G"
pp_class M = red $ text "M"
pp_class T = magenta $ text "T"
pp_class StP = red $ text "P"
pp_class StF = red $ text "F"
pp_class StS = red $ text "S"

--codecolor Code{..} = cc cls code
cc G 0 = dullyellow
cc G 1 = yellow
cc _ _ = red

pp_axis (des, val) =
       (bold $ axiscolor des $ text $ show des)
    <> (cyan $ text $ T.unpack $ sformat sf val)

axiscolor X = red
axiscolor Y = green
axiscolor Z = yellow
axiscolor A = red
axiscolor B = green
axiscolor C = blue
axiscolor E = magenta

pp_axes [] = empty
pp_axes x = space <> pp_list pp_axis x

pp_param (des, val) =
       (bold $ blue $ text $ show des)
    <> (white $ text $ T.unpack $ sformat sf val)

pp_params [] = empty
pp_params x = space <> pp_list pp_param x

pp_comment "" = empty
pp_comment  c = space <> pp_comment' c
pp_comment' "" = empty
pp_comment' c = (dullwhite $ parens $ text $ unpack c)

pp_code Code{..} =
       (cc cls code $ bold $ pp_class cls)
    <> (cc cls code $ text $ show code)
    <> (pp_axes $ M.toList axes)
    <> (pp_params $ M.toList params)
    <> pp_comment comment
pp_code (Comment x) = pp_comment' x
pp_code (Other x) = dullred $ text $ unpack x
