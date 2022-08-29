{-# OPTIONS --without-K #-}

module Ffi.Hs.System.Mem.Weak where

open import Agda.Primitive

private
    variable
        aℓ : Level

postulate
    Weak : Set aℓ → Set aℓ