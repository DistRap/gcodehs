module Data.GCode.Line (
    Line(..)
  , LineType(..)
  , toLines
  , prettyLine
  ) where

{-
Conversion to `Line` from one set of points to another.
Useful for GCode visualisation tools.
-}

import Data.GCode.Canon (Canon(..), CanonState(..))
import Data.GCode.Types (Axes, Style(..))

import Text.PrettyPrint.ANSI.Leijen

import qualified Data.GCode.Pretty

-- | Given two states of `Canon` interpreter output `Line` or empty list
-- if no line is produced by this `Canon`.
toLines :: CanonState -> CanonState -> Canon -> [Line]
toLines prevS nextS code | isTravelMove code || isSetCoords code = pure $ Line
  (travelMoveType code)
  (canonPosition prevS)
  (canonPosition nextS)
toLines _ _ _ | otherwise = mempty

data LineType =
    LineTraverse -- ^ Travel move
  | LineDrawing  -- ^ Machining/drawing move
  | LineJump     -- ^ Produced by set coordinates `SetCoords`
  deriving (Eq, Show, Ord)

data Line = Line
  LineType -- ^ Travel, drawing or set coordinates move
  Axes     -- ^ Start points
  Axes     -- ^ End points
  deriving (Eq, Show, Ord)

-- | Pretty print `Line`
prettyLine :: Style -> Line -> String
prettyLine style x = displayS ((renderer style) (ppLine style x)) ""
  where renderer style' | styleColorful style' == True = renderPretty 0.4 80
        renderer _ =  renderCompact

ppLine :: Style -> Line -> Doc
ppLine style (Line typ from to) =
     ppTyp typ
  <+> string "from"
  <+> Data.GCode.Pretty.ppAxesMap style from
  <+> string "to"
  <+> Data.GCode.Pretty.ppAxesMap style to
  where
    ppTyp LineTraverse = char ' '
    ppTyp LineDrawing  = char '*'
    ppTyp LineJump     = char '>'

-- Helpers

isTravelMove :: Canon -> Bool
isTravelMove (StraightTraverse _) = True
isTravelMove (StraightFeed _)     = True
isTravelMove _                    = False

isSetCoords :: Canon -> Bool
isSetCoords (SetCoords _) = True
isSetCoords _             = False

travelMoveType :: Canon -> LineType
travelMoveType (StraightTraverse _) = LineTraverse
travelMoveType (StraightFeed _)     = LineDrawing
travelMoveType (SetCoords    _)     = LineJump
travelMoveType _                    = error "travelMoveType: Not a travel move"
