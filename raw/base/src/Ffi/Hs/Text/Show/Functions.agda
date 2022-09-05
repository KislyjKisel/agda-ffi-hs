{-# OPTIONS --without-K #-}

module Ffi.Hs.Text.Show.Functions where

open import Ffi.Hs.-base.Class using (Show)

postulate
    Show[A⟶B] : ∀{aℓ bℓ} {A : Set aℓ} {B : Set bℓ} → Show (A → B)

{-# FOREIGN GHC
import Text.Show.Functions
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaShow)
#-}
{-# COMPILE GHC Show[A⟶B] = \ aℓ bℓ a b -> AgdaShow #-}
