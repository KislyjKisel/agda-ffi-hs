{-# OPTIONS --without-K #-}

module Ffi.Hs.Text.Read where

open import Agda.Primitive

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

postulate
    Read : Set aℓ → Set aℓ
    readsPrec:  