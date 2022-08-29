{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Eq where

open import Agda.Primitive
open import Ffi.Hs.Data.Bool using (Bool)

open import Ffi.Hs.-base.Class public
    using (Eq)

private
    variable
        aℓ : Level
        A : Set aℓ

infix 4 _==_ _/=_

postulate
    _==_ : ⦃ Eq A ⦄ → A → A → Bool
    _/=_ : ⦃ Eq A ⦄ → A → A → Bool

{-# COMPILE GHC _==_ = \ aℓ a AgdaEq -> (==) #-}
{-# COMPILE GHC _/=_ = \ aℓ a AgdaEq -> (/=) #-}
