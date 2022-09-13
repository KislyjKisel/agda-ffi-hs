{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Scientific where

open import Agda.Builtin.Bool                   using (Bool)
open import Agda.Builtin.Char                   using (Char)
open import Agda.Builtin.Int                    using () renaming (Int to Integer)
open import Agda.Builtin.List                   using (List)
open import Agda.Builtin.Maybe                  using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Control.DeepSeq              using (NFData)
open import Ffi.Hs.Data.Either                  using (Either)
open import Ffi.Hs.Data.Int                     using (Int)
open import Ffi.Hs.Data.Tuple                   using (Tuple2)
open import Ffi.Hs.GHC.Real                     using (Rational)
open import Ffi.Hs.Text.ParserCombinators.ReadP using (ReadP)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Scientific
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Control.DeepSeq (AgdaNFData)
#-}

private
    variable
        aℓ : Level
        A I R : Set aℓ

postulate
    Scientific : Set

    Eq[Scientific]         : Eq Scientific
    Fractional[Scientific] : Fractional Scientific
    Data[Scientific]       : Data Scientific
    Num[Scientific]        : Num Scientific
    Ord[Scientific]        : Ord Scientific
    Read[Scientific]       : Read Scientific
    Real[Scientific]       : Real Scientific
    RealFrac[Scientific]   : RealFrac Scientific
    Show[Scientific]       : Show Scientific
    NFData[Scientific]     : NFData Scientific
    -- todo: binary -> Binary
    -- todo: hashable -> Hashable
    -- todo: template-haskell -> Lift

{-# COMPILE GHC Scientific = type Data.Scientific.Scientific #-}

{-# COMPILE GHC Eq[Scientific]         = AgdaEq         #-}
{-# COMPILE GHC Fractional[Scientific] = AgdaFractional #-}
{-# COMPILE GHC Data[Scientific]       = AgdaData       #-}
{-# COMPILE GHC Num[Scientific]        = AgdaNum        #-}
{-# COMPILE GHC Ord[Scientific]        = AgdaOrd        #-}
{-# COMPILE GHC Read[Scientific]       = AgdaRead       #-}
{-# COMPILE GHC Real[Scientific]       = AgdaReal       #-}
{-# COMPILE GHC RealFrac[Scientific]   = AgdaRealFrac   #-}
{-# COMPILE GHC Show[Scientific]       = AgdaShow       #-}
{-# COMPILE GHC NFData[Scientific]     = AgdaNFData     #-}

data FPFormat : Set where
    Exponent : FPFormat
    Fixed    : FPFormat
    Generic  : FPFormat

{-# COMPILE GHC FPFormat = data Data.Scientific.FPFormat
    ( Data.Scientific.Exponent
    | Data.Scientific.Fixed
    | Data.Scientific.Generic
    )  #-}

postulate
    Enum[FPFormat] : Enum FPFormat
    Read[FPFormat] : Read FPFormat
    Show[FPFormat] : Show FPFormat

{-# COMPILE GHC Enum[FPFormat] = AgdaEnum #-}
{-# COMPILE GHC Read[FPFormat] = AgdaRead #-}
{-# COMPILE GHC Show[FPFormat] = AgdaShow #-}

postulate
    scientific                    : Integer → Int → Scientific
    coefficient                   : Scientific → Integer
    base10Exponent                : Scientific → Int
    isFloating                    : Scientific → Bool
    isInteger                     : Scientific → Bool
    unsafeFromRational            : Rational → Scientific
    fromRationalRepetend          : Maybe Int → Rational → Either (Tuple2 Scientific Rational) (Tuple2 Scientific (Maybe Int))
    fromRationalRepetendLimited   : Int → Rational → Either (Tuple2 Scientific Rational) (Tuple2 Scientific (Maybe Int))
    fromRationalRepetendUnlimited : Rational → Tuple2 Scientific (Maybe Int)
    toRationalRepetend            : Scientific → Int → Rational
    floatingOrInteger             : ⦃ RealFloat R ⦄ → ⦃ Integral I ⦄ → Scientific → Either R I
    toRealFloat                   : ⦃ RealFloat A ⦄ → Scientific → A
    toBoundedRealFloat            : ⦃ RealFloat A ⦄ → Scientific → Either A A
    toBoundedInteger              : ⦃ Integral I ⦄ → ⦃ Bounded I ⦄ → Scientific → Maybe I
    fromFloatDigits               : ⦃ RealFloat A ⦄ → A → Scientific
    scientificP                   : ReadP Scientific
    formatScientific              : FPFormat → Maybe Int → Scientific → List Char
    toDecimalDigits               : Scientific → Tuple2 (List Int) Int
    normalize                     : Scientific → Scientific

{-# COMPILE GHC scientific                    =                                           Data.Scientific.scientific                    #-}
{-# COMPILE GHC coefficient                   =                                           Data.Scientific.coefficient                   #-}
{-# COMPILE GHC base10Exponent                =                                           Data.Scientific.base10Exponent                #-}
{-# COMPILE GHC isFloating                    =                                           Data.Scientific.isFloating                    #-}
{-# COMPILE GHC isInteger                     =                                           Data.Scientific.isInteger                     #-}
{-# COMPILE GHC unsafeFromRational            =                                           Data.Scientific.unsafeFromRational            #-}
{-# COMPILE GHC fromRationalRepetend          =                                           Data.Scientific.fromRationalRepetend          #-}
{-# COMPILE GHC fromRationalRepetendLimited   =                                           Data.Scientific.fromRationalRepetendLimited   #-}
{-# COMPILE GHC fromRationalRepetendUnlimited =                                           Data.Scientific.fromRationalRepetendUnlimited #-}
{-# COMPILE GHC toRationalRepetend            =                                           Data.Scientific.toRationalRepetend            #-}
{-# COMPILE GHC floatingOrInteger             = \ rℓ r iℓ i AgdaRealFloat AgdaIntegral -> Data.Scientific.floatingOrInteger             #-}
{-# COMPILE GHC toRealFloat                   = \ aℓ a AgdaRealFloat                   -> Data.Scientific.toRealFloat                   #-}
{-# COMPILE GHC toBoundedRealFloat            = \ aℓ a AgdaRealFloat                   -> Data.Scientific.toBoundedRealFloat            #-}
{-# COMPILE GHC toBoundedInteger              = \ iℓ i AgdaIntegral AgdaBounded        -> Data.Scientific.toBoundedInteger              #-}
{-# COMPILE GHC fromFloatDigits               = \ aℓ a AgdaRealFloat                   -> Data.Scientific.fromFloatDigits               #-}
{-# COMPILE GHC scientificP                   =                                           Data.Scientific.scientificP                   #-}
{-# COMPILE GHC formatScientific              =                                           Data.Scientific.formatScientific              #-}
{-# COMPILE GHC toDecimalDigits               =                                           Data.Scientific.toDecimalDigits               #-}
{-# COMPILE GHC normalize                     =                                           Data.Scientific.normalize                     #-}
