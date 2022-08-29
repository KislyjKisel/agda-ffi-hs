{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Ratio where

open import Ffi.Hs.-base.Real using (RealFrac)

open Ffi.Hs.-base.Real public
    using (Ratio; Rational; _%_; numerator; denominator)

postulate
    approxRational : ∀{aℓ} {A : Set aℓ} → ⦃ RealFrac A ⦄ → A → A → Rational

{-# FOREIGN GHC import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Real (AgdaRealFrac(AgdaRealFrac)) #-}
{-# FOREIGN GHC import qualified Data.Ratio #-}
{-# COMPILE GHC approxRational = \ aℓ a AgdaRealFrac -> Data.Ratio.approxRational #-}
