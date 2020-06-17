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

import Data.Map (Map)
import Data.ByteString (ByteString)

import qualified Data.ByteString.Char8
import qualified Data.Double.Conversion.Text
import qualified Data.Map
import qualified Data.Text

import Data.GCode.Types
import Text.PrettyPrint.ANSI.Leijen

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
  where renderer style' | styleColorful style' == True = renderPretty 0.4 80
        renderer _ =  renderCompact

-- | Pretty-print single 'Code' with specified `Style`
ppGCodeLineStyle :: Style -> Code -> String
ppGCodeLineStyle style res = displayS ((renderer style) (ppCode style res)) ""
  where renderer style' | styleColorful style' == True = renderPretty 0.4 80
        renderer _ =  renderCompact

ppList :: (a -> Doc) -> [a] -> Doc
ppList pp x = hsep $ map pp x

ppGCode' :: Style -> [Code] -> Doc
ppGCode' style code = (vsep $ map (ppCode style) code) <> hardline

ppMaybe :: (t -> Doc) -> Maybe t -> Doc
ppMaybe pp (Just x) = pp x
ppMaybe _  Nothing = empty

ppMaybeClass :: Maybe Class -> Doc
ppMaybeClass = ppMaybe ppClass

ppClass :: Class -> Doc
ppClass G           = yellow $ text "G"
ppClass M           = red $ text "M"
ppClass T           = magenta $ text "T"
ppClass PStandalone = red $ text "P"
ppClass FStandalone = red $ text "F"
ppClass SStandalone = red $ text "S"

ccMaybes :: (Eq a, Num a) => Maybe Class -> Maybe a -> Doc -> Doc
ccMaybes (Just cls') (Just num') = cc cls' num'
ccMaybes _ _ = id

cc :: (Eq a, Num a) => Class -> a -> Doc -> Doc
cc G 0 = dullyellow
cc G 1 = yellow
cc _ _ = red

ppAxis :: Style -> (AxisDesignator, Double) -> Doc
ppAxis style (des, val) =
       bold (axisColor des $ text $ show des)
    <> cyan (
          text
        $ Data.Text.unpack
        $ Data.Double.Conversion.Text.toFixed (stylePrecision style) val
        )

axisColor :: AxisDesignator -> Doc -> Doc
axisColor X = red
axisColor Y = green
axisColor Z = yellow
axisColor A = red
axisColor B = green
axisColor C = blue
axisColor E = magenta
axisColor _ = id

ppAxes :: Style -> [(AxisDesignator, Double)] -> Doc
ppAxes style x = ppList (ppAxis style) x

ppAxesMap :: Style -> Map AxisDesignator Double -> Doc
ppAxesMap style x = ppList (ppAxis style) (Data.Map.toList x)

ppParam :: Show a => Style -> (a, Double) -> Doc
ppParam style (des, val) =
       bold (blue $ text $ show des)
    <> white (
          text
        $ Data.Text.unpack
        $ Data.Double.Conversion.Text.toFixed (stylePrecision style) val
        )

ppParams :: Show a => Style -> [(a, Double)] -> Doc
ppParams _ [] = empty
ppParams style x = space <> ppList (ppParam style) x

ppComment :: ByteString -> Doc
ppComment "" = empty
ppComment  c = space <> ppComment' c

ppComment' :: ByteString -> Doc
ppComment' "" = empty
ppComment' c = dullwhite $ parens $ text $ Data.ByteString.Char8.unpack c

ppCode :: Style -> Code -> Doc
ppCode style Code{..} =
       ccMaybes codeCls codeNum ( bold $ ppMaybeClass codeCls)
    <> ccMaybes codeCls codeNum ( ppMaybe (text . show) codeNum)
    <> ppMaybe (\x -> (text ".") <> (text $ show x)) codeSub
    <> ifNonEmpty (\x -> space <> ppAxesMap style x) codeAxes
    <> ppParams style (Data.Map.toList codeParams)
    <> ppComment codeComment
ppCode _ (Comment x) = ppComment' x
ppCode _ (Other x) = dullred $ text $ Data.ByteString.Char8.unpack x
ppCode _ (Empty) = empty
{-# INLINE ppCode #-}

ifNonEmpty :: (Eq t, Monoid t)
           => (t -> Doc)
           -> t -> Doc
ifNonEmpty _ x | x == mempty = empty
ifNonEmpty f x | otherwise   = f x
