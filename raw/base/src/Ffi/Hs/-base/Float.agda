{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Float where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.Int   using () renaming (Int to Integer)
open import Agda.Primitive
open import Ffi.Hs.Data.Tuple using (Tuple2)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Data.Ord    using (Ord)

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    pi                : ⦃ Floating A ⦄ → A
    exp log sqrt      : ⦃ Floating A ⦄ → A → A
    sin cos tan       : ⦃ Floating A ⦄ → A → A
    asin acos atan    : ⦃ Floating A ⦄ → A → A
    sinh cosh tanh    : ⦃ Floating A ⦄ → A → A
    asinh acosh atanh : ⦃ Floating A ⦄ → A → A
    _**_ logBase      : ⦃ Floating A ⦄ → A → A → A
    log1p expm1       : ⦃ Floating A ⦄ → A → A
    log1pexp log1mexp : ⦃ Floating A ⦄ → A → A
    log1mexpOrd       : ⦃ Ord A ⦄ → ⦃ Floating A ⦄ → A → A

    floatRadix     : ⦃ RealFloat A ⦄ → A → Integer
    floatDigits    : ⦃ RealFloat A ⦄ → A → Int
    floatRange     : ⦃ RealFloat A ⦄ → A → Tuple2 Int Int
    decodeFloat    : ⦃ RealFloat A ⦄ → A → Tuple2 Integer Int
    encodeFloat    : ⦃ RealFloat A ⦄ → Integer → Int → A
    exponent       : ⦃ RealFloat A ⦄ → A → Int
    significand    : ⦃ RealFloat A ⦄ → A → A
    scaleFloat     : ⦃ RealFloat A ⦄ → Int → A → A
    isNaN          : ⦃ RealFloat A ⦄ → A → Bool
    isInfinite     : ⦃ RealFloat A ⦄ → A → Bool
    isDenormalized : ⦃ RealFloat A ⦄ → A → Bool
    isNegativeZero : ⦃ RealFloat A ⦄ → A → Bool
    isIEEE         : ⦃ RealFloat A ⦄ → A → Bool
    atan2          : ⦃ RealFloat A ⦄ → A → A → A
