{-| GCode pretty-printing functions

Please do note that these are extremely slow as they do conversion
from ByteStrings to Text and vice-verse. Float formatting is probably
not the fastest as well. Colorfull versions are especially slow.

-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.GCode.Pretty(ppGCode, ppGCodeLine, ppGCodeCompact, ppGCodeLineCompact) where

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Maybe

import Text.PrettyPrint.ANSI.Leijen
import Data.GCode.Types
import Data.GCode.Utils

import Data.Double.Conversion.Text

-- |Pretty-print 'GCode' using colors
ppGCode :: GCode -> String
ppGCode = ppGCodeStyle defaultStyle

-- |Pretty-print 'GCode' using colors with custom floating precision width
ppGCodeStyle :: Style -> GCode -> String
ppGCodeStyle style res = displayS (renderPretty 0.4 80 (ppGCode' style res)) ""

-- |Pretty-print single 'Code' using colors
ppGCodeLine :: Code -> String
ppGCodeLine = ppGCodeLineStyle defaultStyle

-- |Pretty-print single 'Code' using colors with custom floating precision width
ppGCodeLineStyle :: Style -> Code -> String
ppGCodeLineStyle style res = displayS (renderPretty 0.4 80 (ppCode style res)) ""

-- |Pretty-print 'GCode' without colors
ppGCodeCompact :: GCode -> String
ppGCodeCompact = ppGCodeCompactStyle defaultStyle

-- |Pretty-print 'GCode' without colors with custom floating precision width
ppGCodeCompactStyle :: Style -> GCode -> String
ppGCodeCompactStyle style res = displayS (renderCompact (ppGCode' style res)) ""

-- |Pretty-print single 'Code' without colors
ppGCodeLineCompact :: Code -> String
ppGCodeLineCompact = ppGCodeLineCompactStyle defaultStyle

-- |Pretty-print single 'Code' without colors with custom floating precision width
ppGCodeLineCompactStyle :: Style -> Code -> String
ppGCodeLineCompactStyle style res = displayS (renderCompact (ppCode style res)) ""

ppList pp x = hsep $ map pp x

ppGCode' style = vsep . map (ppCode style)

ppMaybe pp (Just x) = pp x
ppMaybe pp Nothing = empty

ppMaybeClass = ppMaybe ppClass

ppClass G = yellow $ text "G"
ppClass M = red $ text "M"
ppClass T = magenta $ text "T"
ppClass StP = red $ text "P"
ppClass StF = red $ text "F"
ppClass StS = red $ text "S"

ccMaybes (Just cls) (Just num) = cc cls num
ccMaybes _ _ = id

cc G 0 = dullyellow
cc G 1 = yellow
cc _ _ = red

ppAxis style (des, val) =
       bold (axisColor des $ text $ show des)
    <> cyan (text $ T.unpack $ toPrecision (stylePrecision style) val)


axisColor X = red
axisColor Y = green
axisColor Z = yellow
axisColor A = red
axisColor B = green
axisColor C = blue
axisColor E = magenta
axisColor _ = id

ppAxes _ [] = empty
ppAxes style x = space <> ppList (ppAxis style) x

ppParam style (des, val) =
       bold (blue $ text $ show des)
    <> white (text $ T.unpack $ toPrecision (stylePrecision style) val)

ppParams _ [] = empty
ppParams style x = space <> ppList (ppParam style) x

ppComment "" = empty
ppComment  c = space <> ppComment' c
ppComment' "" = empty
ppComment' c = dullwhite $ parens $ text $ unpack c

ppCode style Code{..} =
       ccMaybes codeCls codeNum ( bold $ ppMaybeClass codeCls)
    <> ccMaybes codeCls codeNum ( ppMaybe (text . show) codeNum)
    <> ppAxes style (M.toList codeAxes)
    <> ppParams style (M.toList codeParams)
    <> ppComment codeComment
ppCode _ (Comment x) = ppComment' x
ppCode _ (Other x) = dullred $ text $ unpack x
ppCode _ (Empty) = empty
{-# INLINE ppCode #-}
