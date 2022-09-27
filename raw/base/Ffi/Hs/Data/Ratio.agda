{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Ratio where

open import Agda.Primitive
open import Ffi.Hs.-base.Class

open import Ffi.Hs.GHC.Real public
    using
    ( Ratio; Rational; _%_; numerator; denominator
    ; Data[Ratio[A]]
    ; Storable[Ratio[A]]
    ; Enum[Ratio[A]]
    ; Num[Ratio[A]]
    ; Read[Ratio[A]]
    ; Fractional[Ratio[A]]
    ; Real[Ratio[A]]
    ; RealFrac[Ratio[A]]
    ; Show[Ratio[A]]
    ; Eq[Ratio[A]]
    ; Ord[Ratio[A]]
    )

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
