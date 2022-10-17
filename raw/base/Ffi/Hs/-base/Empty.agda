{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Empty where

data ⊥ : Set where

⊥-elim : ∀{aℓ} {A : Set aℓ} → ⊥ → A
⊥-elim ()
