{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Eq where

open import Agda.Primitive
open import Ffi.Hs.Data.Bool using (Bool)

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    Eq : Set aℓ → Set aℓ

infix 4 _==_ _/=_

postulate
    _==_ : ⦃ Eq A ⦄ → A → A → Bool
    _/=_ : ⦃ Eq A ⦄ → A → A → Bool

{-# FOREIGN GHC data AgdaEq aℓ a = Eq a => AgdaEq #-}
{-# COMPILE GHC Eq = type(0) AgdaEq #-}

{-# COMPILE GHC _==_ = \ aℓ a AgdaEq -> (==) #-}
{-# COMPILE GHC _/=_ = \ aℓ a AgdaEq -> (/=) #-}
