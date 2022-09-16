{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Vect where

open import Agda.Primitive

{-# WARNING_ON_IMPORT "unimplemented: requires preprocessor" #-}

postulate
    V2 : ∀{aℓ} → Set aℓ → Set aℓ
    V4 : ∀{aℓ} → Set aℓ → Set aℓ
    Point : ∀{aℓ bℓ} → (Set aℓ → Set bℓ) → Set aℓ → Set (aℓ ⊔ bℓ)
