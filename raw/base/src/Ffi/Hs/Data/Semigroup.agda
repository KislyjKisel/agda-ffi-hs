{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Semigroup where

open import Agda.Primitive
open import Ffi.Hs.-base.Real using (Integral)
open import Ffi.Hs.Data.List.NonEmpty  using (NonEmpty)
open import Ffi.Hs.Data.Monoid         using (Monoid; Endo)

open import Ffi.Hs.-base.Semigroup public
    using (Semigroup)

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

infixr 6 _<>_

postulate
    _<>_      : ⦃ Semigroup A ⦄ → A → A → A
    sconcat   : ⦃ Semigroup A ⦄ → NonEmpty A → A
    stimes    : ⦃ Semigroup A ⦄ → ⦃ Integral B ⦄ → B → A → A

    stimesMonoid           : ⦃ Integral B ⦄ → ⦃ Monoid A ⦄ → B → A → A
    stimesIdempotent       : ⦃ Integral B ⦄ → B → A → A
    stimesIdempotentMonoid : ⦃ Integral B ⦄ → ⦃ Monoid A ⦄ → B → A → A
    mtimesDefault          : ⦃ Integral B ⦄ → ⦃ Monoid A ⦄ → B → A → A

    Min    : Set aℓ → Set aℓ
    mkMin  : A → Min A
    getMin : Min A → A

    Max    : Set aℓ → Set aℓ
    mkMax  : A → Max A
    getMax : Max A → A

    First    : Set aℓ → Set aℓ
    mkFirst  : A → First A
    getFirst : First A → A

    Last    : Set aℓ → Set aℓ
    mkLast  : A → Last A
    getLast : Last A → A

    diff : ⦃ Semigroup A ⦄ → A → Endo A
    cycle1 : ⦃ Semigroup A ⦄ → A → A
