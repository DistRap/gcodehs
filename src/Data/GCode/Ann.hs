{-# LANGUAGE DeriveFunctor #-}

module Data.GCode.Ann (
    Ann(..)
  , stripAnnotation
  ) where

{-
Type for annotating `Code` or `Canon` with source positions.
-}


data Ann a = SrcLine Integer a
  deriving (Show, Eq, Ord, Functor)

stripAnnotation :: Ann a -> a
stripAnnotation (SrcLine _ x) = x
