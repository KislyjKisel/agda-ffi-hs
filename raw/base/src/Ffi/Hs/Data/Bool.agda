{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Bool where

open import Ffi.Hs.-base.Kind using (IsKind)

open import Agda.Builtin.Bool public
    using    (Bool; true; false)

{-# FOREIGN GHC
import qualified Data.Bool
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Kind (AgdaIsKind)
#-}

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

postulate
    `Bool  : Set₁
    `true  : `Bool
    `false : `Bool

{-# COMPILE GHC `Bool  = type Data.Bool.Bool #-}
{-# COMPILE GHC `true  = type 'True          #-}
{-# COMPILE GHC `false = type 'False         #-}

lift`Bool : Bool → `Bool
lift`Bool true  = `true
lift`Bool false = `false

postulate
    IsKind[`Bool] : IsKind `Bool

{-# COMPILE GHC IsKind[`Bool] = AgdaIsKind #-}
