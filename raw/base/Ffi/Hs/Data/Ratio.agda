{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Ratio where

open import Agda.Primitive
open import Ffi.Hs.-base.Class

open import Ffi.Hs.GHC.Real public
    using (Ratio; Rational; _%_; numerator; denominator)

{-# FOREIGN GHC
import qualified Data.Ratio
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    approxRational : ⦃ RealFrac A ⦄ → A → A → Rational

{-# COMPILE GHC approxRational = \ aℓ a AgdaRealFrac -> Data.Ratio.approxRational #-}

postulate
    Data[Ratio[A]        : ⦃ Data A ⦄ → ⦃ Integral A ⦄ → Data (Ratio A)
    Storable[Ratio[A]]   : ⦃ Storable A ⦄ → ⦃ Integral A ⦄ → Storable (Ratio A)
    Enum[Ratio[A]]       : ⦃ Integral A ⦄ → Enum (Ratio A)
    Num[Ratio[A]]        : ⦃ Integral A ⦄ → Num (Ratio A)
    Read[Ratio[A]]       : ⦃ Integral A ⦄ → ⦃ Read A ⦄ → Read (Ratio A)
    Fractional[Ratio[A]] : ⦃ Integral A ⦄ → Fractional (Ratio A)
    Real[Ratio[A]]       : ⦃ Integral A ⦄ → Real (Ratio A)
    RealFrac[Ratio[A]]   : ⦃ Integral A ⦄ → RealFrac (Ratio A)
    Show[Ratio[A]]       : ⦃ Show A ⦄ → Show (Ratio A)
    Eq[Ratio[A]]         : ⦃ Eq A ⦄ → Eq (Ratio A)
    Ord[Ratio[A]]        : ⦃ Integral A ⦄ → Ord (Ratio A)

{-# COMPILE GHC Data[Ratio[A]        = \ aℓ a AgdaData AgdaIntegral     -> AgdaData       #-}
{-# COMPILE GHC Storable[Ratio[A]]   = \ aℓ a AgdaStorable AgdaIntegral -> AgdaStorable   #-}
{-# COMPILE GHC Enum[Ratio[A]]       = \ aℓ a AgdaIntegral              -> AgdaEnum       #-}
{-# COMPILE GHC Num[Ratio[A]]        = \ aℓ a AgdaIntegral              -> AgdaNum        #-}
{-# COMPILE GHC Read[Ratio[A]]       = \ aℓ a AgdaIntegral AgdaRead     -> AgdaRead       #-}
{-# COMPILE GHC Fractional[Ratio[A]] = \ aℓ a AgdaIntegral              -> AgdaFractional #-}
{-# COMPILE GHC Real[Ratio[A]]       = \ aℓ a AgdaIntegral              -> AgdaReal       #-}
{-# COMPILE GHC RealFrac[Ratio[A]]   = \ aℓ a AgdaIntegral              -> AgdaRealFrac   #-}
{-# COMPILE GHC Show[Ratio[A]]       = \ aℓ a AgdaShow                  -> AgdaShow       #-}
{-# COMPILE GHC Eq[Ratio[A]]         = \ aℓ a AgdaEq                    -> AgdaEq         #-}
{-# COMPILE GHC Ord[Ratio[A]]        = \ aℓ a AgdaIntegral              -> AgdaOrd        #-}
