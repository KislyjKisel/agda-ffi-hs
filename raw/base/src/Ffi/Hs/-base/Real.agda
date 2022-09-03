{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Real where

open import Agda.Builtin.Bool          using (Bool)
open import Agda.Builtin.List          using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class         using (Num; Real; Integral; Fractional; RealFrac; Enum; Bounded)
open import Ffi.Hs.-base.Num           using (Integer)
open import Ffi.Hs.Data.Int            using (Int)
open import Ffi.Hs.Data.Ord            using (Ord)
open import Ffi.Hs.Data.Tuple          using (Tuple2)
open import Ffi.Hs.Data.Word           using (Word)
open import Ffi.Hs.Numeric.Natural     using (Natural)
open import Ffi.Hs.Text.Show           using (Show; ShowS)

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

data Ratio (A : Set aℓ) : Set aℓ where
    _:%_ : A → A → Ratio A

Rational : Set
Rational = Ratio Integer

data FractionalExponentBase : Set where
    Base2 Base10 : FractionalExponentBase

infixl 7 _%_ _/_
infixr 8 _^_ _^^_

postulate
    divZeroError              : A
    ratioZeroDenominatorError : A
    overflowError             : A
    underflowError            : A

    numerator   : Ratio A → A
    denominator : Ratio A → A
    
    infinity   : Rational
    notANumber : Rational

    ratioPrec  : Int
    ratioPrec1 : Int

    toRational : ⦃ Real A ⦄ → A → Rational

    quot      : ⦃ Integral A ⦄ → A → A → A
    rem       : ⦃ Integral A ⦄ → A → A → A
    div       : ⦃ Integral A ⦄ → A → A → A
    mod       : ⦃ Integral A ⦄ → A → A → A
    quotRem   : ⦃ Integral A ⦄ → A → A → Tuple2 A A
    divMod    : ⦃ Integral A ⦄ → A → A → Tuple2 A A
    toInteger : ⦃ Integral A ⦄ → A → Integer

    _%_    : ⦃ Integral A ⦄ → A → A → Ratio A
    reduce : ⦃ Integral A ⦄ → A → A → Ratio A

    _/_          : ⦃ Fractional A ⦄ → A → A → A
    recip        : ⦃ Fractional A ⦄ → A → A
    fromRational : ⦃ Fractional A ⦄ → Rational → A

    properFraction : ⦃ RealFrac A ⦄ → ⦃ Integral B ⦄ → A → Tuple2 B A
    truncate       : ⦃ RealFrac A ⦄ → ⦃ Integral B ⦄ → A → B
    round          : ⦃ RealFrac A ⦄ → ⦃ Integral B ⦄ → A → B
    ceiling        : ⦃ RealFrac A ⦄ → ⦃ Integral B ⦄ → A → B
    floor          : ⦃ RealFrac A ⦄ → ⦃ Integral B ⦄ → A → B

    numericEnumFrom       : ⦃ Fractional A ⦄ → A → List A
    numericEnumFromThen   : ⦃ Fractional A ⦄ → A → A → List A
    numericEnumFromTo     : ⦃ Ord A ⦄ → ⦃ Fractional A ⦄ → A → A → List A
    numericEnumFromThenTo : ⦃ Ord A ⦄ → ⦃ Fractional A ⦄ → A → A → A → List A

    Real[Int]         : Real Int
    Integral[Int]     : Integral Int
    Real[Word]        : Real Word
    Integral[Word]    : Integral Word
    Real[Integer]     : Real Integer
    Real[Natural]     : Real Natural
    Integral[Integer] : Integral Integer
    Integral[Natural] : Integral Natural

    Ord[Ratio[A]]        : ⦃ Integral A ⦄ → Ord (Ratio A)
    Num[Ratio[A]]        : ⦃ Integral A ⦄ → Num (Ratio A)
    Fractional[Ratio[A]] : ⦃ Integral A ⦄ → Fractional (Ratio A)
    Real[Ratio[A]]       : ⦃ Integral A ⦄ → Real (Ratio A)
    RealFrac[Ratio[A]]   : ⦃ Integral A ⦄ → RealFrac (Ratio A)
    Show[Ratio[A]]       : ⦃ Show A ⦄ → Show (Ratio A)
    Enum[Ratio[A]]       : ⦃ Integral A ⦄ → Enum (Ratio A)

    fromIntegral : ⦃ Integral A ⦄ → ⦃ Num B ⦄ → A → B
    realToFrac   : ⦃ Real A ⦄ → ⦃ Fractional B ⦄ → A → B

    showSigned : ⦃ Real A ⦄ → (A → ShowS) → Int → A → ShowS
    even       : ⦃ Integral A ⦄ → A → Bool
    odd        : ⦃ Integral A ⦄ → A → Bool

    _^_     : ⦃ Num A ⦄ → ⦃ Integral B ⦄ → A → B → A
    _^^_    : ⦃ Fractional A ⦄ → ⦃ Integral B ⦄ → A → B → A
    _^%^_   : ⦃ Integral A ⦄ → Rational → A → Rational
    _^^%^^_ : ⦃ Integral A ⦄ → Rational → A → Rational

    gcd : ⦃ Integral A ⦄ → A → A → A
    lcm : ⦃ Integral A ⦄ → A → A → A
    
    integralEnumFrom       : ⦃ Integral A ⦄ → ⦃ Bounded A ⦄ → A → List A
    integralEnumFromThen   : ⦃ Integral A ⦄ → ⦃ Bounded A ⦄ → A → A → List A
    integralEnumFromTo     : ⦃ Integral A ⦄ → A → A → List A
    integralEnumFromThenTo : ⦃ Integral A ⦄ → A → A → A → List A

    Show[FractionalExponentBase] : Show FractionalExponentBase

    mkRationalBase2            : Rational → Integer → Rational
    mkRationalBase10           : Rational → Integer → Rational
    mkRationalWithExponentBase : Rational → Integer → FractionalExponentBase → Rational
