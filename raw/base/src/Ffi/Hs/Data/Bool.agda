{-# OPTIONS --without-K --safe #-}

module Ffi.Hs.Data.Bool where

open import Agda.Builtin.Bool public
    using    (Bool; true; false)

infixr 6 _&&_
infixr 5 _||_

_&&_ : Bool → Bool → Bool
true  && x = x
false && x = false

_||_ : Bool → Bool → Bool
true  || x = true
false || x = x

bool : ∀{aℓ} {A : Set aℓ} → A → A → Bool → A
bool x y false = x
bool x y true  = y

not : Bool → Bool
not true  = false
not false = true

otherwise = true
