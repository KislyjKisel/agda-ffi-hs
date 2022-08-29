{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Enum where

open import Agda.Builtin.List           using (List)
open import Agda.Primitive
open import Ffi.Hs.Data.Tuple using (Tuple2)
open import Ffi.Hs.Data.Int             using (Int)
open import Ffi.Hs.Data.String          using (String)
open import Ffi.Hs.Text.Show            using (Show)

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

postulate
    minBound : ⦃ Bounded A ⦄ → A
    maxBound : ⦃ Bounded A ⦄ → A

    succ           : ⦃ Enum A ⦄ → A → A
    pred           : ⦃ Enum A ⦄ → A → A
    toEnum         : ⦃ Enum A ⦄ → Int → A
    fromEnum       : ⦃ Enum A ⦄ → A → Int
    enumFrom       : ⦃ Enum A ⦄ → A → List A
    enumFromThen   : ⦃ Enum A ⦄ → A → A → List A
    enumFromTo     : ⦃ Enum A ⦄ → A → A → List A
    enumFromThenTo : ⦃ Enum A ⦄ → A → A → A → List A

    boundedEnumFrom     : ⦃ Enum A ⦄ → ⦃ Bounded A ⦄ → A → List A
    boundedEnumFromThen : ⦃ Enum A ⦄ → ⦃ Bounded A ⦄ → A → A → List A
    toEnumError         : ⦃ Show A ⦄ → String → Int → Tuple2 A A → B
    fromEnumError       : ⦃ Show A ⦄ → String → A → B
    succError           : String → A
    predError           : String → A
