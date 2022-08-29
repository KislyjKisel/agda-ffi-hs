{-# OPTIONS --without-K #-}

module Ffi.Hs.Text.Show where

open import Agda.Builtin.Bool using (Bool)
open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.List using (List)
open import Agda.Primitive
open import Ffi.Hs.Data.Int   using (Int)

private
    variable
        aℓ : Level
        A : Set aℓ

ShowS : Set
ShowS = List Char → List Char

postulate
    Show      : Set aℓ → Set aℓ
    showsPrec : ⦃ Show A ⦄ → Int → A → ShowS
    show      : ⦃ Show A ⦄ → A → List Char
    showList  : ⦃ Show A ⦄ → List A → ShowS

    shows        : ⦃ Show A ⦄ → A → ShowS
    showChar     : Char → ShowS
    showString   : List Char → ShowS
    showParen    : Bool → ShowS → ShowS
    showListWith : (A → ShowS) → List A → ShowS
