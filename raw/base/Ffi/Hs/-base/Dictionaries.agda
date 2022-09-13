{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Dictionaries where

import Ffi.Hs.-base.Class

-- Generated haskell modules export everything defined
-- This module re-exports only dictionaries for unqualified usage

{-# FOREIGN GHC
import qualified MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class as AgdaBaseClass

pattern AgdaNum        = AgdaBaseClass.AgdaNum
pattern AgdaIntegral   = AgdaBaseClass.AgdaIntegral
pattern AgdaReal       = AgdaBaseClass.AgdaReal
pattern AgdaFractional = AgdaBaseClass.AgdaFractional
pattern AgdaRealFrac   = AgdaBaseClass.AgdaRealFrac
pattern AgdaFloating   = AgdaBaseClass.AgdaFloating
pattern AgdaRealFloat  = AgdaBaseClass.AgdaRealFloat

pattern AgdaBits       = AgdaBaseClass.AgdaBits
pattern AgdaFiniteBits = AgdaBaseClass.AgdaFiniteBits

pattern AgdaRead = AgdaBaseClass.AgdaRead
pattern AgdaShow = AgdaBaseClass.AgdaShow

pattern AgdaBounded = AgdaBaseClass.AgdaBounded
pattern AgdaEnum    = AgdaBaseClass.AgdaEnum
pattern AgdaEq      = AgdaBaseClass.AgdaEq
pattern AgdaOrd     = AgdaBaseClass.AgdaOrd
pattern AgdaIx      = AgdaBaseClass.AgdaIx

pattern AgdaSemigroup = AgdaBaseClass.AgdaSemigroup
pattern AgdaMonoid    = AgdaBaseClass.AgdaMonoid

pattern AgdaFoldable    = AgdaBaseClass.AgdaFoldable
pattern AgdaTraversable = AgdaBaseClass.AgdaTraversable

pattern AgdaFunctor       = AgdaBaseClass.AgdaFunctor
pattern AgdaContravariant = AgdaBaseClass.AgdaContravariant
pattern AgdaApplicative   = AgdaBaseClass.AgdaApplicative
pattern AgdaAlternative   = AgdaBaseClass.AgdaAlternative
pattern AgdaMonad         = AgdaBaseClass.AgdaMonad
pattern AgdaMonadPlus     = AgdaBaseClass.AgdaMonadPlus
pattern AgdaMonadFail     = AgdaBaseClass.AgdaMonadFail
pattern AgdaMonadFix      = AgdaBaseClass.AgdaMonadFix
pattern AgdaMonadZip      = AgdaBaseClass.AgdaMonadZip
pattern AgdaMonadIO       = AgdaBaseClass.AgdaMonadIO

pattern AgdaCategory    = AgdaBaseClass.AgdaCategory
pattern AgdaArrow       = AgdaBaseClass.AgdaArrow
pattern AgdaArrowZero   = AgdaBaseClass.AgdaArrowZero
pattern AgdaArrowChoice = AgdaBaseClass.AgdaArrowChoice
pattern AgdaArrowApply  = AgdaBaseClass.AgdaArrowApply
pattern AgdaArrowPlus   = AgdaBaseClass.AgdaArrowPlus
pattern AgdaArrowLoop   = AgdaBaseClass.AgdaArrowLoop

pattern AgdaBifunctor     = AgdaBaseClass.AgdaBifunctor
pattern AgdaBifoldable    = AgdaBaseClass.AgdaBifoldable
pattern AgdaBitraversable = AgdaBaseClass.AgdaBitraversable

pattern AgdaStorable  = AgdaBaseClass.AgdaStorable
pattern AgdaException = AgdaBaseClass.AgdaException

pattern AgdaData         = AgdaBaseClass.AgdaData
pattern AgdaTypeable     = AgdaBaseClass.AgdaTypeable
pattern AgdaTestEquality = AgdaBaseClass.AgdaTestEquality
pattern AgdaTestCoercion = AgdaBaseClass.AgdaTestCoercion
pattern AgdaCoercible    = AgdaBaseClass.AgdaCoercible

pattern AgdaIsString = AgdaBaseClass.AgdaIsString

pattern AgdaEq1   = AgdaBaseClass.AgdaEq1
pattern AgdaOrd1  = AgdaBaseClass.AgdaOrd1
pattern AgdaRead1 = AgdaBaseClass.AgdaRead1
pattern AgdaShow1 = AgdaBaseClass.AgdaShow1

pattern AgdaEq2   = AgdaBaseClass.AgdaEq2
pattern AgdaOrd2  = AgdaBaseClass.AgdaOrd2
pattern AgdaRead2 = AgdaBaseClass.AgdaRead2
pattern AgdaShow2 = AgdaBaseClass.AgdaShow2

#-}
