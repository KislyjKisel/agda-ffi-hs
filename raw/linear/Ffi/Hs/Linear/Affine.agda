{-# OPTIONS --without-K #-}

module Ffi.Hs.Linear.Affine where

open import Agda.Primitive

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Linear.Affine
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level

data Point (F : Set aℓ → Set aℓ) (A : Set aℓ) : Set aℓ where
    P : F A → Point F A

{-# FOREIGN GHC type AgdaPoint aℓ = Linear.Affine.Point #-}
{-# COMPILE GHC Point = data(1) AgdaPoint (Linear.Affine.P) #-}
