{-| GCode pretty-printing functions

Please do note that these are extremely slow as they do conversion
from ByteStrings to Text and vice-verse. Float formatting is probably
not the fastest as well. Colorfull versions are especially slow.

-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.GCode.Pretty(
    ppGCode
  , ppGCodeLine
  , ppGCodeCompact
  , ppGCodeLineCompact
  , ppGCodeStyle
  , ppGCodeLineStyle
  , ppAxes
  , ppAxesMap
  ) where

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Maybe

import Text.PrettyPrint.ANSI.Leijen
import Data.GCode.Types
import Data.GCode.Utils

import Data.Double.Conversion.Text

-- | Pretty-print 'GCode' using colors
ppGCode :: GCode -> String
ppGCode = ppGCodeStyle (defaultStyle { styleColorful = True })

-- | Pretty-print single 'Code' using colors
ppGCodeLine :: Code -> String
ppGCodeLine = ppGCodeLineStyle (defaultStyle { styleColorful = True })

-- | Pretty-print 'GCode' without colors
ppGCodeCompact :: GCode -> String
ppGCodeCompact = ppGCodeStyle defaultStyle

-- | Pretty-print single 'Code' without colors
ppGCodeLineCompact :: Code -> String
ppGCodeLineCompact = ppGCodeLineStyle defaultStyle

-- | Pretty-print 'GCode' with specified `Style`
ppGCodeStyle :: Style -> GCode -> String
ppGCodeStyle style res = displayS ((renderer style) (ppGCode' style res)) ""
  where renderer style | styleColorful style == True = renderPretty 0.4 80
        renderer _ =  renderCompact

-- | Pretty-print single 'Code' with specified `Style`
ppGCodeLineStyle :: Style -> Code -> String
ppGCodeLineStyle style res = displayS ((renderer style) (ppCode style res)) ""
  where renderer style | styleColorful style == True = renderPretty 0.4 80
        renderer _ =  renderCompact

ppList pp x = hsep $ map pp x

ppGCode' style code = (vsep $ map (ppCode style) code) <> hardline

ppMaybe pp (Just x) = pp x
ppMaybe pp Nothing = empty

ppMaybeClass = ppMaybe ppClass

ppClass G           = yellow $ text "G"
ppClass M           = red $ text "M"
ppClass T           = magenta $ text "T"
ppClass PStandalone = red $ text "P"
ppClass FStandalone = red $ text "F"
ppClass SStandalone = red $ text "S"

ccMaybes (Just cls) (Just num) = cc cls num
ccMaybes _ _ = id

cc G 0 = dullyellow
cc G 1 = yellow
cc _ _ = red

ppAxis style (des, val) =
       bold (axisColor des $ text $ show des)
    <> cyan (text $ T.unpack $ toFixed (stylePrecision style) val)

axisColor X = red
axisColor Y = green
axisColor Z = yellow
axisColor A = red
axisColor B = green
axisColor C = blue
axisColor E = magenta
axisColor _ = id

ppAxes style x = ppList (ppAxis style) x

ppAxesMap style x = ppList (ppAxis style) (M.toList x)

ppParam style (des, val) =
       bold (blue $ text $ show des)
    <> white (text $ T.unpack $ toFixed (stylePrecision style) val)

ppParams _ [] = empty
ppParams style x = space <> ppList (ppParam style) x

ppComment "" = empty
ppComment  c = space <> ppComment' c
ppComment' "" = empty
ppComment' c = dullwhite $ parens $ text $ unpack c

ppCode style Code{..} =
       ccMaybes codeCls codeNum ( bold $ ppMaybeClass codeCls)
    <> ccMaybes codeCls codeNum ( ppMaybe (text . show) codeNum)
    <> ppMaybe (\x -> (text ".") <> (text $ show x)) codeSub
    <> ifNonEmpty (\x -> space <> ppAxesMap style x) codeAxes
    <> ppParams style (M.toList codeParams)
    <> ppComment codeComment
ppCode _ (Comment x) = ppComment' x
ppCode _ (Other x) = dullred $ text $ unpack x
ppCode _ (Empty) = empty
{-# INLINE ppCode #-}

ifNonEmpty :: (Eq t, Monoid t)
           => (t -> Doc)
           -> t -> Doc
ifNonEmpty _ x | x == mempty = empty
ifNonEmpty f x | otherwise   = f x
